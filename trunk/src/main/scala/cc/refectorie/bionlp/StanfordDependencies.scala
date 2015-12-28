package cc.refectorie.bionlp

import scala.io.Source
import System.err
import cc.factorie._
import collection.immutable.HashSet
import scala.util.Sorting
import collection.mutable.{MultiMap, ArrayBuffer, HashMap}
import java.io.{PrintStream, BufferedInputStream, FileInputStream}

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object StanfordDependencies {
  var tokenToDepForward = new HashMap[Token, HashMap[Token, SDependency]]
  var tokenToDepBackward = new HashMap[Token, HashMap[Token, SDependency]]

  val tokenToDepForwardMc = new HashMap[Token, HashMap[Token, SDependency]]
  val tokenToDepBackwardMc = new HashMap[Token, HashMap[Token, SDependency]]

  def resetTrees = {
    tokenToDepBackward.clear
    tokenToDepForward.clear
  }

  def fixTrees(doc: Document) {
    val proteins = new scala.collection.mutable.HashSet[Token]
    for (sentence: Sentence <- doc; gene <- sentence.genes; token <- gene.span) proteins += token
    val tree = new DependencyTree(tokenToDepForward,tokenToDepBackward)
    //tree.fixConjunctsWithProteins(proteins)
    for (sentence <- doc; token:Token <- sentence){
      if (token.hasNext && token.next.hasNext && token.next.word == "-") {
        val dep =  token.next.next.word match {
          case x if (x.charAt(0).isLowerCase) => "hyphen-lc"
          case x if (x.charAt(0).isDigit) => "hypen-digit"
          case _ => "hyphen-norm"
        }
        tree.addDependency(token.next.next,token, dep)
      }
      if (token.hasNext && token.next.hasNext && token.next.word == "/") {
        tree.addDependency(token.next.next,token, "slash")
      }
      if (token.word == "in" && token.hasPrev) {
        tree.addDependency(token.prev,token, "in")
      }
    }
    //tree.fixPPAttachments(proteins)
    tokenToDepForward = tree.tokenToDepForward
    tokenToDepBackward = tree.tokenToDepBackward
  }



  class DependencyTree {
    val tokenToDepForward = new HashMap[Token, HashMap[Token, SDependency]]
    val tokenToDepBackward = new HashMap[Token, HashMap[Token, SDependency]]


    def this(tokenToDepForward: HashMap[Token, HashMap[Token, SDependency]],
             tokenToDepBackward: HashMap[Token, HashMap[Token, SDependency]]) {
      this()
      for (entry <- tokenToDepForward) {
        val clone = new HashMap[Token, SDependency]
        clone ++= entry._2
        this.tokenToDepForward(entry._1) = clone
      }
      for (entry <- tokenToDepBackward) {
        val clone = new HashMap[Token, SDependency]
        clone ++= entry._2
        this.tokenToDepBackward(entry._1) = clone
      }
    }
    def this(tree:DependencyTree){
      this(tree.tokenToDepForward,tree.tokenToDepBackward)
     }
    

    def copy = {
      new DependencyTree(tokenToDepForward, tokenToDepBackward)
    }

    def removeDependency(head: Token, modifier: Token) = {
      tokenToDepForward(head).removeKey(modifier)
      tokenToDepBackward(modifier).removeKey(head)
    }

    def addDependency(head: Token, modifier: Token, dep: String) {
      addDependency(head,modifier, new SDependency(head.sentence.doc,
        head.sentence,head.position, modifier.position,dep))
    }

    def addDependency(head: Token, modifier: Token, dep: SDependency) {
      tokenToDepForward.getOrElseUpdate(head, new HashMap[Token, SDependency])(modifier) = dep
      tokenToDepBackward.getOrElseUpdate(modifier, new HashMap[Token, SDependency])(head) = dep
    }

    //fixes conjuncts with proteins and non-proteins a la Poon 2010
    def fixConjunctsWithProteins(tokensWithProteins: scala.collection.Set[Token]) {
      //in find conjunctions to fix

      val conjunctionsToRemove = new ArrayBuffer[(Token, Token)]
      val attachmentsToMake = new ArrayBuffer[(Token, Token, SDependency)]

      for (modifier <- tokenToDepBackward.keySet;
           head <- tokenToDepBackward(modifier).keySet) {
        val dep = tokenToDepBackward(modifier)(head)

        if (dep.dependencyType.startsWith("conj")) {
          if (tokensWithProteins(head) != tokensWithProteins(modifier)) {
            val (protein, noProtein) = if (tokensWithProteins(head)) (head, modifier) else (modifier, head)
            //does noProtein have a protein child
            val proteinChild = tokenToDepForward.get(noProtein).flatMap(
              _.keySet.find(child => child != protein && tokensWithProteins(child)))
            if (proteinChild.isDefined) {
              conjunctionsToRemove += (head, modifier)
              val d = tokenToDepForward(noProtein)(proteinChild.get)
              //todo: set token indices
              attachmentsToMake += (noProtein, protein,
                      new SDependency(d.doc, d.sentence, noProtein.position, protein.position, d.dependencyType))

            }
          }
          //
        }
      }
      //now remove conjunctions
      for (toRemove <- conjunctionsToRemove) {
        System.err.println("Removing dependency: %s->%s".format(toRemove._1, toRemove._2))
        removeDependency(toRemove._1, toRemove._2)
      }
      //now re-attach
      for (toAttach <- attachmentsToMake) {
        System.err.println("Attaching dependency: %s-(%s)->%s".format(
          toAttach._1, toAttach._3.dependencyType, toAttach._2))
        addDependency(toAttach._1, toAttach._2, toAttach._3)
      }
    }

    def head(modifier: Token): Option[Token] = {
      tokenToDepBackward.get(modifier).map(_.keys.next)
    }

    def heads(modifier: Token): scala.collection.Set[Token] = {
      tokenToDepBackward.getOrElse(modifier, new HashMap[Token, SDependency]).keySet
    }

    def rightMostDescendentsLeftOf(head: Token, pivot: Int): Seq[Token] = {
      var maxPos = -1
      var rightMost: Token = null
      for (children <- tokenToDepForward.get(head)) {
        for (child: Token <- children.keySet) {
          if (child.position > maxPos) {
            rightMost = child
            maxPos = child.position
          }
        }
      }
      if (rightMost == null)
        Nil
      else if (rightMost.position < pivot)
        Seq(rightMost) ++ rightMostDescendentsLeftOf(rightMost, pivot)
      else
        rightMostDescendentsLeftOf(rightMost, pivot)
    }

    //fix pp attachments a la Poon 2010 using AttachmentPatterns
    def fixPPAttachments(tokensWithProteins: scala.collection.Set[Token]) {

      val toRemove = new ArrayBuffer[(Token, Token)]
      val toAttach = new ArrayBuffer[(Token, Token, SDependency)]

      for (modifier <- tokenToDepBackward.keySet;
           head <- tokenToDepBackward(modifier).keySet) {
        val dep = tokenToDepBackward(modifier)(head)
        if (dep.dependencyType.startsWith("prep")) {
          //collect head candidates
          val candidates = new ArrayBuffer[Token]
          candidates += head
          candidates ++= this.head(head)
          candidates ++= rightMostDescendentsLeftOf(head, modifier.position)
          val sorted = candidates.toList.sort((x, y) =>
            Math.abs(modifier.position - x.position) < Math.abs(modifier.position - y.position))
          AttachmentPatterns.findMatch(modifier, dep, sorted, tokensWithProteins) match {
            case Some(token) => {
              toRemove += (head, modifier)
              toAttach += (token, modifier, new SDependency(dep.doc, dep.sentence,
                token.position, modifier.position, dep.dependencyType))
            }
            case None => {}
          }
        }
      }
      //now remove conjunctions
      for (toRemove <- toRemove) {
        removeDependency(toRemove._1, toRemove._2)
      }
      //now re-attach
      for (toAttach <- toAttach) {
        addDependency(toAttach._1, toAttach._2, toAttach._3)
      }

    }


  }




  // cleans up all the weird tokenization string from parser
  def cleanPString(s: String): String = {
    s.replaceAll("-LSB-", "[").
            replaceAll("-RSB-", "]").
            replaceAll("-LRB-", "(").
            replaceAll("-RRB-", ")").
            replaceAll("''", "\"")
  }

  // generates a feature from the dependency graph between these featuresWithStats and if array is not empty single ngram featuresWithStats
  def featureFromPath(t1: Token, t2: Token, singleNGrams: Array[Int], prefixPathNGrams: Array[Int], mc: Boolean, preStr: String): Array[String] = {
    // DEBUG XXX
    //    if (t1.word=="inhibit" || t2.word=="inhibit") {
    //      System.err.println(t1+"==="+t2)
    //      val sentence = t1.sentence
    //      System.err.println(sentence.dependencyPaths(t1))
    //      System.err.println(sentence.dependencyPaths(t1)(t2))
    //    }
    val prefix = if (mc) preStr + "SDRETOK" else preStr + "SD"
    if (t1.sentence != t2.sentence) {System.err.println("Over sentence borders, no, no, no! We do not do that!")}
    val sentence = t1.sentence
    val dependencyPaths = if (mc) sentence.dependencyPathsMc else sentence.dependencyPaths
    var ρ = new ArrayBuffer[String]
    if (dependencyPaths.contains(t1) && dependencyPaths(t1).contains(t2) && dependencyPaths(t1)(t2) != null) {
      val path = dependencyPaths(t1)(t2)
      val featurePath = path.map((sd: (SDependency, Boolean)) => sd._1.dependencyType + (if (sd._2) "↑" else "↓"))
      ρ += prefix + "-PATH=" + featurePath.mkString("→")
      // ngrams
      if (singleNGrams != null) {
        for (π <- singleNGrams; φ <- 0 to featurePath.length - π; if (π != 0)) {
          ρ += prefix + "-" + π + "Gram=" + ((for (κ <- 0 until π) yield featurePath(φ + κ)).mkString("→"))
        }
      }
      // prefix on path
      if (prefixPathNGrams != null) {
        for (π <- prefixPathNGrams; if (π != 0)) {
          ρ += prefix + "-" + π + "Prefix=" + ((for (κ <- 0 until π; if (κ < featurePath.length)) yield featurePath(κ)).mkString("→"))
        }
      }
    }
    ρ.toArray
  }


  def attachToDoc(sdFilename: String, doc: Document) = {
    // output?
    val printCurrentSentence = false
    val debug = false
    // while running through the lines, we run through all sentences in document
    var currentSentenceId = 0
    var currentSentence = doc(currentSentenceId)
    if (printCurrentSentence) System.err.println(currentSentence)
    if (debug) System.err.println("Reading SD from " + sdFilename)
    val fis = new FileInputStream(sdFilename)
    val bis = new BufferedInputStream(fis)
    // val source = Source.fromFile(sdFilename) // gives too many files open error
    val source = Source.fromInputStream(bis)
    val lines = source.getLines
    for (sdLine <- lines) {
      if (debug) System.err.println("SDLine " + sdLine)
      //      System. err.println(sdLine)
      if (sdLine.length == 1) {
        if (currentSentenceId < doc.size - 1)
          currentSentenceId += 1
        currentSentence = doc(currentSentenceId)
        if (printCurrentSentence) System.err.println(currentSentence)
        //        System.err.println(currentSentence)
      }
      // each line in the file contains two tokens, in that format: dobj(induces-3, phosphorylation-4)
      val fbi = sdLine.indexOf("(");
      val csi = sdLine.indexOf(", ")
      if (csi != -1 && fbi != -1) {
        val tokenOne = sdLine.substring(fbi + 1, csi)
        val tokenTwo = sdLine.substring((csi + 2), sdLine.length - 2)
        val tohi = tokenOne.lastIndexOf('-')
        val tthi = tokenTwo.lastIndexOf('-')
        val tokenOneText = cleanPString(tokenOne.substring(0, tohi))
        val tokenTwoText = cleanPString(tokenTwo.substring(0, tthi))
        // a 28' occurs as token number in the data... therefore `.replaceAll("[^0-9]","")`
        val tokenOneIndex = tokenOne.substring(tohi + 1, tokenOne.length).replaceAll("[^0-9]", "").toInt - 1
        val tokenTwoIndex = tokenTwo.substring(tthi + 1, tokenTwo.length).replaceAll("[^0-9]", "").toInt - 1
        val dependence = sdLine.substring(0, fbi)
        if (debug) System.err.println(tokenOneText + "==" + currentSentence(tokenOneIndex).word + ":" + (tokenOneText.equals(currentSentence(tokenOneIndex).word)))
        if (debug) System.err.println(tokenTwoText + "==" + currentSentence(tokenTwoIndex).word + ":" + (tokenTwoText.equals(currentSentence(tokenTwoIndex).word)))
        if (debug) System.err.println(tokenOneText + ":" + tokenTwoText + ":" + tokenOneIndex + ":" + tokenTwoIndex + ":" + dependence + "-->" + currentSentence(tokenOneIndex) + ":" + currentSentence(tokenTwoIndex))
        // this check fails due to token replacement in parser, eg. [ is replaced by -LSB-
        if ((!tokenOneText.equals(currentSentence(tokenOneIndex).word)) || (!tokenTwoText.equals(currentSentence(tokenTwoIndex).word))) {
          System.err.println("*** Token counts of stanford dependencies and sentences do not match")
          //throw new Exception("Token counts of stanford dependencies and sentences do not match! " + "(" + (tokenOneText) + "!=" + (currentSentence(tokenOneIndex).word) + ")" + "(" + (tokenTwoText) + "!=" + (currentSentence(tokenTwoIndex).word) + ")" + currentSentence)
        }
        val sd = new SDependency(doc, currentSentence, tokenOneIndex, tokenTwoIndex, dependence)
        // add to index of dependencies of doc
        // add to inverse index
        // tokenToDependency.get(currentSentence(tokenOneIndex))
        // tokenToDependency.put(currentSentence(tokenTwoIndex),sd)
        // forward
        if (!tokenToDepForward.contains(currentSentence(tokenOneIndex))) tokenToDepForward += currentSentence(tokenOneIndex) -> new HashMap[Token, SDependency]
        tokenToDepForward(currentSentence(tokenOneIndex)) += currentSentence(tokenTwoIndex) -> sd
        // backward
        if (!tokenToDepBackward.contains(currentSentence(tokenTwoIndex))) tokenToDepBackward += currentSentence(tokenTwoIndex) -> new HashMap[Token, SDependency]
        tokenToDepBackward(currentSentence(tokenTwoIndex)) += currentSentence(tokenOneIndex) -> sd
      }
    }
    //doc.depTree = new DependencyTree(tokenToDepForward,tokenToDepBackward)
    bis.close
    fis.close
  }

  def attachToDocWithReToc(sdFilename: String, doc: Document) {
    attachToDocWithReToc(sdFilename,doc,tokenToDepForwardMc,tokenToDepBackwardMc)
  }


  def attachToDocWithReToc(sdFilename: String, doc: Document,
          tokenToDepForwardMc:HashMap[Token, HashMap[Token, SDependency]],
          tokenToDepBackwardMc:HashMap[Token, HashMap[Token, SDependency]]) {
    // output?
    val printCurrentSentence = false
    val debug1 = false
    val debug = false
    // while running through the lines, we run through all sentences in document
    var currentSentenceId = 0
    var currentSentence = doc(currentSentenceId)
    if (printCurrentSentence) System.err.println(currentSentence)
    var dependenciesForDoc = new ArrayBuffer[ArrayBuffer[(String, String, String, Int, Int, Int, Int)]] // dependency, tok1text, tok2text, t1index, t2index, t1plus, t2plus
    dependenciesForDoc += new ArrayBuffer[(String, String, String, Int, Int, Int, Int)]
    var dependenciesForDocReToc = new ArrayBuffer[ArrayBuffer[(String, String, String, Int, Int, Int, Int)]] // dependency, tok1text, tok2text, t1index, t2index, t1plus, t2plus
    for (sdLine <- Source.fromFile(sdFilename).getLines) {
      if (debug1) System.err.println(sdLine)
      //      System. err.println(sdLine)
      if (sdLine.length == 1) {
        if (currentSentenceId < doc.size - 1)
          currentSentenceId += 1
        currentSentence = doc(currentSentenceId)
        if (printCurrentSentence) System.err.println(currentSentence)
        dependenciesForDoc += new ArrayBuffer[(String, String, String, Int, Int, Int, Int)]
      }
      // each line in the file contains two tokens, in that format: dobj(induces-3, phosphorylation-4)
      val fbi = sdLine.indexOf("(");
      val csi = sdLine.indexOf(", ")
      if (csi != -1 && fbi != -1) {
        val tokenOne = sdLine.substring(fbi + 1, csi)
        val tokenTwo = sdLine.substring((csi + 2), sdLine.length - 2)
        val tohi = tokenOne.lastIndexOf('-')
        val tthi = tokenTwo.lastIndexOf('-')
        val tokenOneText = cleanPString(tokenOne.substring(0, tohi))
        val tokenTwoText = cleanPString(tokenTwo.substring(0, tthi))
        // a 28' occurs as token number in the data... therefore `.replaceAll("[^0-9]","")`
        val tokenOneIndex = tokenOne.substring(tohi + 1, tokenOne.length).replaceAll("[^0-9]", "").toInt - 1
        val tokenTwoIndex = tokenTwo.substring(tthi + 1, tokenTwo.length).replaceAll("[^0-9]", "").toInt - 1
        val dependence = sdLine.substring(0, fbi)
        if (debug1) System.err.println(tokenOneText + "==" + currentSentence(tokenOneIndex).word + ":" + (tokenOneText.equals(currentSentence(tokenOneIndex).word)))
        if (debug1) System.err.println(tokenTwoText + "==" + currentSentence(tokenTwoIndex).word + ":" + (tokenTwoText.equals(currentSentence(tokenTwoIndex).word)))
        if (debug1) System.err.println(tokenOneText + ":" + tokenTwoText + ":" + tokenOneIndex + ":" + tokenTwoIndex + ":" + dependence + "-->" + currentSentence(tokenOneIndex) + ":" + currentSentence(tokenTwoIndex))

        dependenciesForDoc.last += (dependence, tokenOneText, tokenTwoText, tokenOneIndex, tokenTwoIndex, 0, 0)
      }
    }


    // now we have the document and can work on it, we do that sentence wise
    for ((dependenciesInSentence: ArrayBuffer[(String, String, String, Int, Int, Int, Int)]) <- dependenciesForDoc) {
      if (debug) System.err.println("--- Next Sentence ---")
      if (debug) System.err.println("Originally:")
      if (debug) dependenciesInSentence.foreach(x => System.err.println(x))

      // to adapt the indices for following tokens
      val retokenized = new HashSet[(Int, Int)] // original index, new length
      // tokenize each token in there, add one to each succeeding token index
      val newDependenciesInSentence = new ArrayBuffer[(String, String, String, Int, Int, Int, Int)]
      for (dependency <- dependenciesInSentence) {
        val (dep, tok1text, tok2text, tok1i, tok2i, plus1, plus2) = dependency
        val tok1retok = Tokenizer.tokenize(tok1text)
        val tok2retok = Tokenizer.tokenize(tok2text)
        if (tok1retok.length != 1) retokenized.addEntry((tok1i, tok1retok.length))
        if (tok2retok.length != 1) retokenized.addEntry((tok2i, tok2retok.length))
        if (tok1retok.length != 1 || tok2retok.length != 1) {
          // add new ones for each new token
          for (i <- tok1retok.length-1 until tok1retok.length; j <- tok2retok.length-1 until tok2retok.length) {
            //System.err.println("Adding: "+(dep,tok1retok(i),tok2retok(j),tok1i,tok2i,i,j))
            newDependenciesInSentence += (dep, tok1retok(i), tok2retok(j), tok1i, tok2i, i, j)
          }
        }
        else { // keep the old thing
          newDependenciesInSentence += (dep, tok1text, tok2text, tok1i, tok2i, 0, 0)
        }
      }

      if (debug) System.err.println("Changed: " + retokenized.mkString(" "))
      // backwards trough the list of changed stuff
      val sortedRetokenizedList = new ArrayBuffer[(Int, Int)]
      retokenized.foreach(x => sortedRetokenizedList += x)
      Sorting.stableSort(sortedRetokenizedList, (a: (Int, Int), b: (Int, Int)) => a._1 < b._1)
      for ((i, l) <- sortedRetokenizedList) {
        if (debug) System.err.println("Changing: " + (i, l))
        // change all which are behind and add it to a new list
        for (dependency <- newDependenciesInSentence) {
          if (debug) System.err.println("From: " + dependency)
          val (dep, tok1text, tok2text, tok1i, tok2i, plus1, plus2) = dependency
          val newplus1 = if (tok1i > i) (plus1 + l - 1) else plus1
          val newplus2 = if (tok2i > i) (plus2 + l - 1) else plus2
          val newDependency = (dep, tok1text, tok2text, tok1i, tok2i, newplus1, newplus2)
          newDependenciesInSentence(newDependenciesInSentence.indexOf(dependency)) = newDependency
          if (debug) System.err.println("To:   " + newDependency)
        }
      }
      // now we collected the information how to shift, let's do it
      for (dependency <- newDependenciesInSentence) {
        val (dep, tok1text, tok2text, tok1i, tok2i, plus1, plus2) = dependency
        val newDependency = (dep, tok1text, tok2text, tok1i + plus1, tok2i + plus2, 0, 0)
        newDependenciesInSentence(newDependenciesInSentence.indexOf(dependency)) = newDependency
      }


      if (debug) newDependenciesInSentence.foreach(x => System.err.println(x))
      if (debug) System.err.println()

      // to attach then:
      dependenciesForDocReToc += newDependenciesInSentence
    }

    // NOW EVERYTHING IS RETOKENIZED, let's attach it
    currentSentence = doc(0)
    for (dependencyOnSentence <- dependenciesForDocReToc) {
      //System.err.println("-------------------------")
      //System.err.println("Using")
      //System.err.println(currentSentence.toString(true))
      //dependencyOnSentence.foreach(x => System.err.println(x))
      for (dependency <- dependencyOnSentence) {
        var (dependence, tokenOneText, tokenTwoText, tokenOneIndex, tokenTwoIndex, plus1, plus2) = dependency
        // work on BEGIN
        // sometimes tokens are not mentioned in SD, but needed to be retokenized, check that and correct that if possible
        if ((!tokenOneText.equals(currentSentence(tokenOneIndex).word)) || (!tokenTwoText.equals(currentSentence(tokenTwoIndex).word))) {
        //  System.err.println("Problem with " + tokenOneText + " != " + currentSentence(tokenOneIndex).word + " or " + tokenTwoText + " != " + currentSentence(tokenTwoIndex).word)
          // something's not good, search for a matching token, it can only be in advance
          // token one
          if ((!tokenOneText.equals(currentSentence(tokenOneIndex).word)))
            for (c <- currentSentence; if (currentSentence.indexOf(c) > tokenOneIndex)) {
              // does it fit?
              if (tokenOneText == c.word) {
                // change the index of the token, that should nearly always work, as we start to search from the prior beginning
                //System.err.println("Found matching at "+currentSentence.indexOf(c)+" ("+c.word+")")
                // use this index now
                tokenOneIndex = currentSentence.indexOf(c)
              }
            }
          //  token two
          if ((!tokenTwoText.equals(currentSentence(tokenTwoIndex).word)))
            for (c <- currentSentence; if (currentSentence.indexOf(c) > tokenTwoIndex)) {
              // does it fit?
              if (tokenTwoText == c.word) {
                // change the index of the token, that should nearly always work, as we start to search from the prior beginning
                //System.err.println("Found matching at "+currentSentence.indexOf(c)+" ("+c.word+")")
                // use this index now
                tokenTwoIndex = currentSentence.indexOf(c)
              }
            }
        } // work on END

        // this check fails due to token replacement in parser, eg. [ is replaced by -LSB-
        if ((!tokenOneText.equals(currentSentence(tokenOneIndex).word)) || (!tokenTwoText.equals(currentSentence(tokenTwoIndex).word))) {
          System.err.println("Original tokenized text:")
          System.err.println(currentSentence.map(_.word).mkString("  "))
          System.err.println("Using:")
          dependencyOnSentence.foreach(x => System.err.println(x))
          System.err.println("*** Token counts of stanford dependencies and sentences do not match! " + "(" + (tokenOneText) + "!=" + (currentSentence(tokenOneIndex).word) + ")" + "(" + (tokenTwoText) + "!=" + (currentSentence(tokenTwoIndex).word) + ")" + currentSentence)
          //throw new Exception("Token counts of stanford dependencies and sentences do not match! " + "(" + (tokenOneText) + "!=" + (currentSentence(tokenOneIndex).word) + ")" + "(" + (tokenTwoText) + "!=" + (currentSentence(tokenTwoIndex).word) + ")" + currentSentence)
        }
        val sd = new SDependency(doc, currentSentence, tokenOneIndex, tokenTwoIndex, dependence)
        // add to index of dependencies of doc
        // add to inverse index
        // tokenToDependency.get(currentSentence(tokenOneIndex))
        // tokenToDependency.put(currentSentence(tokenTwoIndex),sd)
        // forward
        if (!tokenToDepForwardMc.contains(currentSentence(tokenOneIndex))) tokenToDepForwardMc += currentSentence(tokenOneIndex) -> new HashMap[Token, SDependency]
        tokenToDepForwardMc(currentSentence(tokenOneIndex)) += currentSentence(tokenTwoIndex) -> sd
        // backward
        if (!tokenToDepBackwardMc.contains(currentSentence(tokenTwoIndex))) tokenToDepBackwardMc += currentSentence(tokenTwoIndex) -> new HashMap[Token, SDependency]
        tokenToDepBackwardMc(currentSentence(tokenTwoIndex)) += currentSentence(tokenOneIndex) -> sd
      }
      if (currentSentence.hasNext)
        currentSentence = currentSentence.next
    }
  }

  def generateSDPath(doc: Document) = {
    if (InitFile.selftokenized || !InitFile.selftokenized && !InitFile.mc) {
      for ((sentence: Sentence) <- doc) {
        //System.err.println("DOING SOMETHING!")
        // for each sentence, we compute all shortest paths
        val (path, next) = fw(sentence, tokenToDepForward, tokenToDepBackward)
        //System.err.println(path.length+"..."+next.length)
        // now get all sd paths between all tokens and store them to the sentence
        for (i <- 0 until sentence.length; j <- 0 until sentence.length; if (i != j)) {
          val sdp = getSDPath(sentence, i, j, path, next, tokenToDepForward, tokenToDepBackward)
          if (!sentence.dependencyPaths.contains(sentence(i))) {
            sentence.dependencyPaths += sentence(i) -> new HashMap[Token, ArrayBuffer[(SDependency, Boolean)]]
            sentence.trueSentence.dependencyPaths += sentence.trueSentence(i) -> new HashMap[Token, ArrayBuffer[(SDependency, Boolean)]]
          }
          sentence.dependencyPaths(sentence(i)) += sentence(j) -> sdp
          sentence.trueSentence.dependencyPaths(sentence.trueSentence(i)) += sentence.trueSentence(j) -> sdp
          //System.err.println("Added "+sdp)
        }
        //sentence.trueSentence.dependencyPaths = sentence.dependencyPaths

      }
    }
    if (InitFile.mc) {
      for ((sentence: Sentence) <- doc) {
        // for each sentence, we compute all shortest paths
        val (path, next) = fw(sentence, tokenToDepForwardMc, tokenToDepBackwardMc)
        // now get all sd paths between all tokens and store them to the sentence
        for (i <- 0 until sentence.length; j <- 0 until sentence.length; if (i != j)) {
          val sdp = getSDPath(sentence, i, j, path, next, tokenToDepForwardMc, tokenToDepBackwardMc)
          if (!sentence.dependencyPathsMc.contains(sentence(i))) sentence.dependencyPathsMc += sentence(i) -> new HashMap[Token, ArrayBuffer[(SDependency, Boolean)]]
          sentence.dependencyPathsMc(sentence(i)) += sentence(j) -> sdp
        }
        sentence.trueSentence.dependencyPathsMc = sentence.dependencyPathsMc
      }
    }
  }

  // instead of this shitty search, lets try Floyd Warshall
  private def fw(sentence: Sentence, forward: HashMap[Token, HashMap[Token, SDependency]], backward: HashMap[Token, HashMap[Token, SDependency]]): (Array[Array[Int]], Array[Array[Int]]) = {
    // init
    val slength = sentence.length
    val path = new Array[Array[Int]](slength, slength)
    val next = new Array[Array[Int]](slength, slength)
    for (i <- 0 until slength; j <- 0 until slength) {
      next(i)(j) = -1
      if ((forward.contains(sentence(i)) && forward(sentence(i)).contains(sentence(j))) || (backward.contains(sentence(i)) && backward(sentence(i)).contains(sentence(j)))) {
        path(i)(j) = 1
      } else {
        path(i)(j) = 99999
      }
    }
    // search
    for (k <- 0 until slength) {
      for (i <- 0 until slength; j <- 0 until slength) {
        //        System.err.println("Checking "+sentence(i).word+"..."+sentence(k).word+"..."+sentence(j).word+": "+path(i)(k)+" + "+path(k)(j) + " < " + path(i)(j)+ " = "+((path(i)(k) + path(k)(j)) < path(i)(j)))
        if ((path(i)(k) + path(k)(j)) < path(i)(j)) {
          path(i)(j) = path(i)(k) + path(k)(j)
          next(i)(j) = k
        }
      }
    }
    return (path, next)
  }

  private def getPath(i: Int, j: Int, path: Array[Array[Int]], next: Array[Array[Int]]): ArrayBuffer[Int] = {
    if (path(i)(j) == 99999) {
      //System.err.println("CASE 1")
      return null
    }
    if (next(i)(j) == -1) {
      //System.err.println("CASE 2")
      val r = new ArrayBuffer[Int]
      //r += j
      return r
    }
    val t = new ArrayBuffer[Int]
    t ++= getPath(i, next(i)(j), path, next)
    t += next(i)(j)
    t ++= getPath(next(i)(j), j, path, next)
    return t
  }

  private def getSDPath(sentence: Sentence, i: Int, j: Int, path: Array[Array[Int]], next: Array[Array[Int]], forward: HashMap[Token, HashMap[Token, SDependency]], backward: HashMap[Token, HashMap[Token, SDependency]]): ArrayBuffer[(SDependency, Boolean)] = {
    val sdpath = new ArrayBuffer[(SDependency, Boolean)]
    val intPath = new ArrayBuffer[Int]
    //System.err.println("Get Path From "+sentence(i)+" to "+sentence(j))
    val tmp = getPath(i, j, path, next)
    if (tmp == null) return null
    intPath += i
    intPath ++= tmp
    intPath += j
    for (n <- 0 until intPath.length - 1) {
      sdpath += (if (forward.contains(sentence(intPath(n))) && forward(sentence(intPath(n))).contains(sentence(intPath(n + 1))))
        (forward(sentence(intPath(n)))(sentence(intPath(n + 1))), true)
      else
        (backward(sentence(intPath(n)))(sentence(intPath(n + 1))), false)
              )
    }
    return sdpath
  }

  def main(args: Array[String]) {
    System.err.println("Init")
    InitFile.initialize("/home/rklinger/work/mercurial/refectorie/proj/bionlp/init/dev.ini")
    System.err.println("Read File")
    val doc = Data.readOneFile("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tiny1/8913871")
    System.err.println("Old SD Read")
    if (InitFile.initKeyValues("selftokenized") == "true") {
      System.err.println("Attaching self tokenized SD to doc...")
      StanfordDependencies.attachToDoc("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tiny1/sd/8913871.txt", doc)
    }
    System.err.println("MC SD Read")
    if (InitFile.initKeyValues("mc") == "true") {
      System.err.println("Attaching MC SD to doc...")
      StanfordDependencies.attachToDocWithReToc("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tiny1/mc-sd/8913871.txt", doc)
    }
    System.err.println("SD Sizes: " + tokenToDepForward + "; " + tokenToDepBackward)
    System.err.println("MC Sizes: " + tokenToDepForwardMc + "; " + tokenToDepBackwardMc)
    System.err.println("Path Generation")
    generateSDPath(doc)
    System.err.println("OUTPUT")
    for (sentence: Sentence <- doc) {
      System.err.println("Sentence: " + sentence.toString(true))
      for (t1 <- sentence; t2 <- sentence; if (t1 != t2)) {
        if (InitFile.initKeyValues("selftokenized") == "true") System.err.println(t1.word + "->" + t2.word + ": " + (if (sentence.dependencyPaths(t1)(t2) != null) sentence.dependencyPaths(t1)(t2).mkString("->") else ""))
        if (InitFile.initKeyValues("mc") == "true") System.err.println(t1.word + "->" + t2.word + ": " + (if (sentence.dependencyPathsMc(t1)(t2) != null) sentence.dependencyPathsMc(t1)(t2).mkString("->") else ""))
        System.err.println
      }
    }
  }
}

class SDependency(val doc: Document, val sentence: Sentence, val tok1: Int, val tok2: Int, val dependencyType: String) {
  override def toString = dependencyType + "(" + sentence(tok1).word + ";" + sentence(tok2).word + ")"

  lazy val from = sentence(tok1)
  lazy val to = sentence(tok2)

  def changeType(newType:String) = new SDependency(doc,sentence,tok1,tok2,newType)
  
}

object AttachmentPatterns {
  val PROTEIN = "PROTEIN"

  val depType2patterns = new HashMap[String, scala.collection.mutable.Set[AttachmentPattern]]
          with MultiMap[String, AttachmentPattern]

  //default pattern
  addPattern(AttachmentPattern("regulation", PROTEIN, "prep_by"))


  def addPattern(pattern: AttachmentPattern) {
    depType2patterns.getOrElseUpdate(pattern.depType, new scala.collection.mutable.HashSet[AttachmentPattern]) += pattern
  }

  def findMatch(token: Token,
                dep: SDependency,
                candidates: Seq[Token],
                proteins: scala.collection.Set[Token]): Option[Token] = {
    for (candidate <- candidates; pattern <- depType2patterns.getOrElse(dep.dependencyType, Set.empty)) {
      if (pattern.isMatch(candidate, token, dep, proteins)) return Some(candidate)
    }
    None
  }

  def main(args: Array[String]) {
    InitFile.initialize(args(0))
    val trainDocs = Data.readCorpus(args(1))
    val out = new PrintStream(args(2))
    val patterns = new scala.collection.mutable.HashSet[String]
    for (doc <- trainDocs;
         sentence: Sentence <- doc;
         event: Event <- sentence.trueSentence.events;
         arg <- event.arguments;
         clueToken <- event.clue.map(t => sentence(t.position));
         argToken <- arg.entity.span.map(t => sentence(t.position))) {
      val paths = sentence.dependencyPaths.get(clueToken);
      if (paths.isDefined) {
        for (path <- paths.get.get(argToken)) {
          if (path != null) {
            if (path.size == 1) {
              if (path(0)._1.dependencyType.startsWith("prep")) {
                val triple = arg.entity match {
                  case e: Event => Seq(clueToken.word.toLowerCase, path(0)._1.dependencyType, argToken.word.toLowerCase)
                  case p: Gene => Seq(clueToken.word.toLowerCase, path(0)._1.dependencyType, "PROTEIN")
                  case _ => error("Cant' be")
                }
                val string = if (path(0)._2) triple.mkString(" ") else triple.reverse.mkString(" ")
                println(string)
                patterns += string
              }
            }
          }
        }
      }


    }

    out.println(patterns.mkString("\n"))
    out.close

  }

}

case class AttachmentPattern(head: String, modifier: String, depType: String) {
  lazy val headIsProtein = head == "PROTEIN"
  lazy val modifierIsProtein = modifier == "PROTEIN"

  def isMatch(headToken: Token, modifierToken: Token, dep: SDependency,
              proteins: scala.collection.Set[Token]) = {
    (headIsProtein && proteins(headToken) || head == headToken.word) &&
            (modifierIsProtein && proteins(modifierToken) || modifier == modifierToken.word) &&
            (dep.dependencyType == depType)
  }
}

object FixTrees {

  def main(args:Array[String]) {
    InitFile.initialize(args(0))
    InitFile.fixTrees = true
    Data.readCorpus(args(1))
  }

}
