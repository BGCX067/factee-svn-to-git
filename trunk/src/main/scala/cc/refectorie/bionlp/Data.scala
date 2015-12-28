package cc.refectorie.bionlp

import cc.factorie._
import scala.io.Source
import collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import java.text.DecimalFormat
import cc.refectorie.bionlp.Templates.{EventFeatureVector, EventTemplate}
import collection.immutable.HashSet
import org.tartarus.snowball.ext.EnglishStemmer
import scala.util.{Sorting, Marshal}
import java.io.{FileInputStream, BufferedInputStream, PrintStream, File}
import cc.refectorie.bionlp.StanfordDependencies.DependencyTree

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

class Entity(var span: Span) {
  /* is this entity the same as another one? by means of span, eventtype, entity class, not arguments!!! */
  def deepEqual(other: Entity): Boolean = {
    ((other.isInstanceOf[Gene] && this.isInstanceOf[Gene] && this.span.deepEqual(other.span)) || // span does not need to be checked for event, deep equal is doing that
            (other.isInstanceOf[Event] && this.isInstanceOf[Event]) && this.asInstanceOf[Event].deepEqual(true, false, false, other.asInstanceOf[Event]))
  }

  override def toString(): String = {
    "Entity=" + (if (this.isInstanceOf[Gene]) "Gene" + this.asInstanceOf[Gene] else "Event" + this.asInstanceOf[Event]) + "; "
  }

  def isUsedAsArgument: Int = {
    val sentence = span.sentence
    sentence.events.filter(
      event => event.arguments.exists(
        arg => arg.entity == this)).length
  }

  def isUsedAsArgumentBoolean: Boolean = {
    val sentence = span.sentence
    sentence.events.exists(
      event => event.arguments.exists(
        arg => arg.entity == this))
  }

  def genericOutputFormatT: String = {
    if (this.isInstanceOf[Gene]) this.asInstanceOf[Gene].outputFormat
    else this.asInstanceOf[Event].outputFormatT
  }

  def genericOutputFormatTorE: String = {
    if (this.isInstanceOf[Gene]) this.asInstanceOf[Gene].outputFormat
    else this.asInstanceOf[Event].outputFormatE
  }
}

class EventType extends LabelVariable[String]("BlaBinding") {
}

class RoleType extends LabelVariable[String]("Theme")

class EventPair(val e1: Event, val e2: Event) {
  override def toString(): String = e1.outputFormatE.trim + " --- " + e2.outputFormatE.trim
}

class Span(val sentence: Sentence, start: Int, length: Int, anEntity: Entity, var id: String, val realtext: String)(implicit d: DiffList) extends SpanVariable[Token](sentence, start, length)(d) {
  def this(sentence: Sentence, start: Int, length: Int, anEntity: Entity, id: String)(implicit d: DiffList) = this (sentence, start, length, anEntity, id, null) (d)

  def hash: String = {
    sentence.doc.id + "; " + sentence.sentenceNumInDoc + "; " + "->" + start + ":" + length + ":"
  }

  //the head of this span
  lazy val head: Token = {
    if (size == 1) first else {
      //find token that has a parent outside of the span
      var found = false
      var tokens = elements
      var result: Token = null
      while (!found && tokens.hasNext) {
        val token = tokens.next
        found = sentence.heads(token).exists(h => !this.contains(h))
        if (found) {
          result = token
          //System.err.println("*** found head of %s: %s".format(stringrepr,result.word))
        }
      }
      if (found) result else last

    }
  }


  lazy val mainClueToken = {
    if (last.word == "-" && size > 1) apply(size - 2)
    else if (InitFile.mainClueTokenHeuristics && last.word == "in" && size > 1) apply(size - 2)
    else last
  }

  var stringrepr = if (realtext != null) realtext else this.map(_.word).mkString(" ")

  var entities = new ArrayBuffer[Entity];
  if (d != null) d += new FlagDiff(sentence)

  if (anEntity != null) {
    addEntity(anEntity)
  }

  def isOfGene(): Boolean = {
    entities.find(e => e.isInstanceOf[Gene]).isDefined
  }

  def isOfEvent(): Boolean = {
    entities.isEmpty || !entities.find(e => e.isInstanceOf[Gene]).isDefined
  }

  def addEntity(x: Entity)(implicit d: DiffList) {
    if (Data.checkEntityHomogenityOnSpans && isOfEvent && x.isInstanceOf[Gene]) throw new Exception("This is the span of an event, do not add a gene!")
    if (Data.checkEntityHomogenityOnSpans && isOfGene && x.isInstanceOf[Event]) throw new Exception("This is the span of an gene, do not add an event!")
    val diff = new AddEntityDiff(x)
    diff.redo // actually adds
    if (d != null) d += diff
  }

  case class AddEntityDiff(x: Entity) extends Diff {
    def undo = {
      entities -= x
    }

    def redo = {
      entities += x
    }

    def variable = x.span.sentence // xxx was Span.this, could be the Sentence?
  }

  // get all events
  def events: Seq[Event] = entities.filter(i => (i.isInstanceOf[Event])).map(i => i.asInstanceOf[Event])

  def genes: Seq[Gene] = entities.filter(i => (i.isInstanceOf[Gene])).map(i => i.asInstanceOf[Gene])
  // output, selects automatically which format should be used, depending on the entity related to this span
  //  def outputFormat() : String = {
  //    var spanStartDocOffset = this.first.docOffsets._1
  //    var spanEndDocOffset = Int.MinValue
  //    for (token <- this) spanEndDocOffset = token.docOffsets._2
  //    val entityString = sentence.getDocumentText.substring(spanStartDocOffset,spanEndDocOffset)
  //
  //    if (genes.length > 0)  // to check if Gene is in, similar to if (entity.isInstanceOf[Gene])
  //      id+"\tProtein "+spanStartDocOffset+" "+spanEndDocOffset+"\t"+entityString+"\n"
  //    else if (events.length > 0) { // questionable but can a gene ever be a clue?
  //      //        val sb = new StringBuilder
  //      //    	  for (eventEntity <- events)
  //      //    		  sb ++= id+"\t"+eventEntity.asInstanceOf[Event].eventType.value+" "+spanStartDocOffset+" "+spanEndDocOffset+"\t"+entityString+"\n"
  //      //    	  sb.toString
  //      id+"\t"+events(0).asInstanceOf[Event].eventType.value+" "+spanStartDocOffset+" "+spanEndDocOffset+"\t"+entityString+"\n"
  //    }
  //    else
  //      id+"\t"+"Entity"+" "+spanStartDocOffset+" "+spanEndDocOffset+"\t"+entityString+"\n"
  //  }

  // get all spanned tokens
  def spannedTokens = for (t <- start to start + length - 1) yield sentence(t)

  /* Compares by means of offset, NOT related entity */
  def deepEqual(other: Span) = {
    other.start == this.start && other.length == this.length // && this.sentence.sentenceTxt.equals(other.sentence.sentenceTxt) // the latter makes it slow, check if necessary
  }

  def isATrueEventSpanInSentence(): Boolean = {
    val res = this.sentence.trueSentence.spans.filter(s => s.isOfEvent).find(x => x.deepEqual(this)) // possible because of searching in true spans
    res.isDefined
  }

  override def toString = "Span:" + start + ";" + length + "(" + first.offsets._1 + ";" + last.offsets._2 + ")" + "\'" + stringrepr + "\""
}

class TrueSentence(start: Int, end: Int, documentTxt: String, val variableSentence: Sentence, sentenceNum: Int, doc: Document) extends Sentence(start: Int, end: Int, documentTxt: String, sentenceNum: Int, doc: Document) {
  def numberOfMissedEvents(checkEventType: Boolean, checkEventTheme: Boolean, checkEventArguments: Boolean, showFNStream: PrintStream): Double = {
    // run through all true events in the trueSentence (this) and see if we got each in the variableSentence
    var fn = 0.0
    val variableEvents = variableSentence.events
    val alreadyCounted = new ArrayBuffer[Event]
    for (trueEvent <- this.events) {
      val varEventOption = variableEvents.find(varEvent => varEvent.deepEqual(checkEventType, checkEventTheme, checkEventArguments, trueEvent) && !alreadyCounted.contains(varEvent))
      if (!varEventOption.isDefined /*|| alreadyCounted.contains(varEventOption.get)*/ ) {
        fn += 1.0
        if (showFNStream != null) showFNStream.println("FN: " + trueEvent + " with arguments " + trueEvent.arguments)
      }
      if (varEventOption.isDefined) alreadyCounted += varEventOption.get
    }
    fn
  }

  def numberOfMissedEventSpans(): Double = {
    // run through all true events in the trueSentence (this) and see if we got each in the variableSentence
    var fp = 0.0
    val variableSpans = variableSentence.spans.filter(s => s.isOfEvent)
    for (trueSpan <- this.spans.filter(s => s.isOfEvent)) { // in true sentence with true spans not as one line above
      val varSpanOption = variableSpans.find(varSpan => varSpan.deepEqual(trueSpan))
      if (!varSpanOption.isDefined) {
        fp += 1.0
      }
    }
    fp
  }
  // to be fast, on the true sentence these should not change after reading the data
  val cachedEvents = new ArrayBuffer[Event]

  override def events: ArrayBuffer[Event] = cachedEvents
}

/* A sentence representation with start and end offsets relative to the whole document.
 * The tokens are inherited from VariableSeqWithSpans
 */
class Sentence(val start: Int, end: Int, val documentTxt: String, val sentenceNumInDoc: Int, val doc: Document) extends VariableSeqWithSpans[Token, Span] with VarInTypedSeq[Sentence, Document] {
  // generate a true sentence, but only if this is not already a true sentence, then we just set it to null
  val trueSentence = if (!this.isInstanceOf[TrueSentence]) new TrueSentence(start, end, documentTxt, this, sentenceNumInDoc, doc) else null
  // offsets relative to the whole document
  val offsets: (Int, Int) = (start, end);
  // Constructor: Get a sentence object with tokenization
  val sentenceTxt = documentTxt.substring(start, end)
  var currentLeft = 0
  var currentRight = -1
  for (t <- Tokenizer.tokenize(sentenceTxt)) {
    currentLeft = sentenceTxt.indexOf(t, currentRight)
    currentRight = currentLeft + t.length
    if (!t.matches("^[ \t\n]*$")) {
      this += new Token(currentLeft, currentRight, t, this) // offsets are relative to the whole document!
    }
    //currentRight = currentLeft
  }
  for (t1 <- this; t2 <- this; if (t1 != t2)) {
    if (t1.offsets._1 == t2.offsets._1)
      error("Cant be: two tokens %s and %s with same offset in doc %s".format(t1.word, t2.word, doc.id))

  }

  lazy val semiCount = {
    val result = new HashMap[Token, Int]
    var count = 0
    for (token <- this) {
      result(token) = count
      if (token.word == ";") count += 1
    }
    result
  }

  // which events do I store
  //val events = new ArrayBuffer[Event] ;
  // automatically retrieve all events from spans on this sequence instead of storing them manually: SLOW
  def events: Seq[Event] = {
    if (this.isInstanceOf[TrueSentence]) this.asInstanceOf[TrueSentence].cachedEvents else {
      for ((span: Span) <- this.spans; (event: Event) <- span.events) yield event
    }
  }

  def genes: Seq[Gene] = {
    for ((span: Span) <- this.spans; (gene: Gene) <- span.genes) yield gene
  }


  def eval(checkEventType: Boolean, checkEventTheme: Boolean, checkEventArguments: Boolean, punishFP: Boolean): (Double, Double, Double, Double, Double, Double) = {
    eval(checkEventType, checkEventTheme, checkEventArguments: Boolean, null, punishFP, false, false)
  }

  // temp score function
  def eval(checkEventType: Boolean, checkEventTheme: Boolean, checkEventArguments: Boolean, stream: PrintStream, punishFP: Boolean, printTPFPFN: Boolean, skipFN: Boolean): (Double, Double, Double, Double, Double, Double) = {
    var tp, fp, fn = 0.0
    // over all events in me
    val alreadyCountedTrueEvents = new HashSet[Event]
    for (event:Event <- events) {
      if (event.present) {
        // how good is this event?
        if (event.isATrueEventInSentence(checkEventType, checkEventTheme, checkEventArguments, alreadyCountedTrueEvents)) {
          tp += 1.0;
          if (printTPFPFN && stream != null) stream.println("TP: " + event + " with arguments " + event.arguments)
        }
        else {
          fp += 1.0;
          if (printTPFPFN && stream != null) stream.println("FP: " + event + " with arguments " + event.arguments)
        }
      }
    }
    // did we miss an event in me
    fn = if (skipFN)
      -1.0
    else
      trueSentence.numberOfMissedEvents(checkEventType, checkEventTheme, checkEventArguments, if (printTPFPFN && stream != null) stream else null)
    val prec = if ((tp + fp) == 0) 1.0 else tp / (tp + fp)
    val rec = if (skipFN) -1.0 else {if ((tp + fn) == 0) 0.0 else tp / (tp + fn)}
    val f1 = if (skipFN) -1.0 else {if ((prec + rec) == 0) 0.0 else 2 * (prec * rec) / (prec + rec)}
    if (stream != null)
      stream.println(tp + "\t" + fp + "\t" + fn + "\t" + prec + "\t" + rec + "\t" + f1)
    //Workflow.deberr("SentenceEvalFunction: Events ("+this.events.length+")-->"+this.events+" returning "+f1)
    val result = if (skipFN) -1.0 else {if (punishFP && f1 < 0.3) f1 - 0.01 * fp else f1} // only punish at the beginning, this threshold is somewhat arbitrary
    (tp, fp, fn, prec, rec, result)
  }


  def evalClue(stream: PrintStream, punishFP: Boolean, beta: Double, skipFN: Boolean): (Double, Double, Double, Double, Double, Double) = {
    var tp, fp, fn = 0.0
    // over all events in me
    for (span <- this.spans.filter(s => s.isOfEvent)) {
      // how good is this event?
      if (span.isATrueEventSpanInSentence())
        tp += 1.0;
      else
        fp += 1.0;
    }
    // did we miss an event in me
    fn = if (skipFN)
      -1.0
    else
      trueSentence.numberOfMissedEventSpans()
    val prec = if ((tp + fp) == 0) 1.0 else tp / (tp + fp)
    val rec = if (skipFN) -1.0 else {if ((tp + fn) == 0) 0.0 else tp / (tp + fn)}
    val fbeta = if (skipFN) -1.0 else {if ((prec + rec) == 0) 0.0 else (1 + beta * beta) * (prec * rec) / (beta * beta * prec + rec)}
    if (stream != null)
      stream.println(tp + "\t" + fp + "\t" + fn + "\t" + prec + "\t" + rec + "\t" + fbeta)
    //Workflow.deberr("SentenceEvalFunction: Events ("+this.events.length+")-->"+this.events+" returning "+f1)
    val result = if (skipFN) -1.0 else {if (punishFP && fbeta < 0.3) fbeta - 0.01 * fp else fbeta} // only punish at the beginning, this threshold is somewhat arbitrary
    //System.err.println((tp,fp,fn,prec,rec,result))
    (tp, fp, fn, prec, rec, result)
  }

  // accuracy on token level
  def acc(checkEventType: Boolean, checkEventTheme: Boolean, checkEventArguments: Boolean): Double = {
    // denominator:
    val allTokensNum: Double = this.length
    var trueResult: Double = 0


    var currentToken = this.first
    var currentTrueToken = this.trueSentence.first
    while (currentToken.hasNext || currentTrueToken.hasNext) { // || such that errors come up
      //println(currentToken+"---"+currentTrueToken)
      if (currentToken.eventsAreTrue(currentTrueToken, checkEventTheme, checkEventType, checkEventArguments)) trueResult += 1
      currentToken = currentToken.next
      currentTrueToken = currentTrueToken.next
    }
    val accuracy = trueResult / allTokensNum
    //System.err.println("ACC="+accuracy)
    accuracy
  }

  // cares about all stuff around, like removing according span and so on
  def removeEvent(event: Event, removeSpanIfEmpty: Boolean)(implicit d: DiffList) = {
    val diff = new RemoveEventDiff(event)
    if (event.span.events.contains(event))
      diff.redo // calls the removement
    else {
      // removed this error message as it can happen without any harm while removing events recursively
      //System.err.println("Tried to remove event "+event+" which did not work! It is still present: "+event.present)
    }
    if (d != null) d += diff
    // remove the span if empty and wished
    if (removeSpanIfEmpty) {
      //val spanOfEvent = this.spans.find((x:Span) => x.entities.find(y => y == event).isDefined)// that was wrong, i think
      if (event.clue.entities.isEmpty) {
        this.removeSpan(event.clue)(d)
      }
    }
    // also remove all arguments (not entities which form thes arguments)
    for (argument <- event.arguments) event.removeEntityAsArgument(argument)(d)
    // remove this event from all events in which it was argument
    //    for((otherEvent:Event) <- event.clue.sentence.events ; if (otherEvent.containsEntityAsArgumentNonDeep(event))) {
    //      val argOfThisEvent = otherEvent.arguments.find(a => a.entity == event).get
    //      otherEvent.removeEntityAsArgument(argOfThisEvent)(d)
    //    }
    // above is not recursiv, this is
    removeEventsAsArgsRecursivly(event)(d)
    event.delete(d)
  }

  def removeEventsAsArgsRecursivly(event: Event)(implicit d: DiffList) {
    for ((otherEvent: Event) <- event.clue.sentence.events; if (otherEvent.containsEntityAsArgumentNonDeep(event))) {
      val argOfThisEventOption = otherEvent.arguments.find(a => a.entity == event)
      if (argOfThisEventOption.isDefined) // TODO check when it could happen that this is not defined!
        otherEvent.removeEntityAsArgument(argOfThisEventOption.get)(d)
      if (otherEvent.getThemes.length == 0) { // during the recursive process, it could happen that it tries to remove the same multiple times. Not good.
        //        removeEventsAsArgsRecursivly(otherEvent)(d)
        removeEvent(otherEvent, false)(d)
      }
    }
  }

  lazy val trueClueRelation:Set[(Int,String)] = Set() ++ trueSentence.events.map(_.clueTypeRelation)
  lazy val trueClueArgRelation:Set[(Int,Int,String)] = Set() ++ trueSentence.events.flatMap(_.clueArgRelation)
  lazy val trueArgPairRelation = Set() ++ trueSentence.events.flatMap(_.argPairRelation)

  case class RemoveEventDiff(event: Event) extends Diff {
    def undo = {
      event.clue.entities += event
    }

    def redo = {
      //val spanOfEvent = Sentence.this.spans.find((x:Span) => x.entities.find(y => y == event).isDefined) // too compilicated
      event.clue.entities -= event
      //spanOfEvent.get.entities -= event
    }

    def variable = Sentence.this // xxx was Sentence.this, could be the Clue?
  }

  // if the clue is given, it is used and the start and length parameters ignored, if not, we make a new clue
  def addNewEvent(clueStart: Int, clueLength: Int, eventTypeValue: String, aClue: Span)(implicit d: DiffList): Event = {
    // given a span?
    // is there a span already?
    val clue = if (clueStart != -1 && clueLength != -1 && aClue == null) {
      val clueOption = this.spans.find(span => span.start == clueStart && span.length == clueLength)
      if (clueOption.isDefined) clueOption.get else new Span(this, clueStart, clueLength, null, "T9999")(d)
    } else {
      aClue
    }
    val eventType = new EventType()
    eventType.set(eventTypeValue)(d)
    val event2 = new Event("E9999", clue, eventType)(d)
    clue.addEntity(event2)(d)
    event2
  }

  def addNewRandomEvent(clueStart: Int)(implicit d: DiffList): Event = {
    addNewEvent(clueStart, 1, Data.possibleEventTypes(Workflow.rand.nextInt(9)), null)(d)
  }

  // how to present myself
  override def toString() = "Sentence:" + start + ";" + end + ": " + documentTxt.substring(start, Math.min(start + 15, end)) + "..."

  def toString(fullSentence: Boolean): String = if (fullSentence) ":" + start + ";" + end + ": " + documentTxt.substring(start, end) else toString()
  // to get the content
  def getText() = documentTxt.substring(start, end)

  def getDocumentText = documentTxt

  def printStructure() = {
    System.err.println(toString(true))
    System.err.println("Events:")
    for (event <- events) {
      System.err.println("    " + event + " with arguments " + event.arguments)
    }
    System.err.println("Genes:")
    for (gene <- genes) {
      System.err.println("    " + gene)
    }
    System.err.println("True Events:")
    for (event <- trueSentence.events) {
      System.err.println("    " + event + " with arguments " + event.arguments)
    }
  }

  var dependencyPaths = new HashMap[Token, HashMap[Token, ArrayBuffer[(SDependency, Boolean)]]]
  var dependencyPathsMc = new HashMap[Token, HashMap[Token, ArrayBuffer[(SDependency, Boolean)]]]

  def heads(modifier: Token): Iterable[Token] = {
    val paths = dependencyPaths.get(modifier)
    if (paths.isDefined && paths.get != null) {
      for (entry <- paths.get; if (entry._2 != null && entry._2.size == 1 && !entry._2(0)._2)) yield entry._1
    } else
      Nil
  }

  def headDeps(modifier: Token): Iterable[(SDependency, Token)] = {
    val paths = dependencyPaths.get(modifier)
    if (paths.isDefined && paths.get != null) {
      for (entry <- paths.get; if (entry._2 != null && entry._2.size == 1 && !entry._2(0)._2)) yield (entry._2(0)._1, entry._1)
    } else
      Nil
  }

  def path(from: Token, to: Token): Option[Seq[(SDependency, Boolean)]] = {
    val path = dependencyPaths.get(from).flatMap(p => p.get(to))
    if (path.isDefined && path.get == null) None else path
  }

}

/* A token knows where it is in the document, has binary featuresWithStats, and is part of a sequence */
class Token(start: Int, end: Int, val word: String, val sentence: Sentence) extends BinaryVectorVariable[String] with VarInTypedSeq[Token, Sentence] {
  val offsets: (Int, Int) = (start, end)
  val docOffsets: (Int, Int) = (start + sentence.offsets._1, end + sentence.offsets._1)
  lazy val sentencePos: Int = {sentence.indexOf(this)}
  val stem = Data.stem(word)
  var pos: String = null
  lazy val isInGene = sentence.spansContaining(this.position).exists(_.isOfGene)
  // feature extraction:
  //	this += word // as word
  //  this += "PREFIX4="+word.substring(0,Math.min(word.length,4)) // prefix4
  //  this += "PREFIX3="+word.substring(0,Math.min(word.length,3)) // prefix3
  //  this += "PREFIX2="+word.substring(0,Math.min(word.length,2)) // prefix2
  //  this += "SUFFIX4="+word.substring(Math.max(word.length-4,0)) // suffix4
  //  this += "SUFFIX3="+word.substring(Math.max(word.length-3,0)) // suffix3
  //  this += "SUFFIX2="+word.substring(Math.max(word.length-2,0)) // suffix2
  //  if (sentence.length <= sentencePos+1) this += "@1<END>" else this ++= sentence(sentencePos+1).values.filter((x:String)=>(!x.contains("@"))).map(x=>"@1"+x) // OC1
  //  if (sentencePos-1 <= 0) this += "@-1<START>" else this ++= sentence(sentencePos-1).values.filter((x:String)=>(!x.contains("@"))).map(x=>"@-1"+x) // OC-1
  //  if (sentence.length <= sentencePos+2) this += "@2<END>" else this ++= sentence(sentencePos+2).values.filter((x:String)=>(!x.contains("@"))).map(x=>"@2"+x) // OC2
  //  if (sentencePos-2 <= 0) this += "@-2<START>" else this ++= sentence(sentencePos-2).values.filter((x:String)=>(!x.contains("@"))).map(x=>"@-2"+x) // OC-2
  // feature extraction end
  override def toString = { // based on the one from BinaryVectorVariable
    val s = new StringBuilder(printName + "[" + start + ";" + end + "]/[" + sentencePos + "]" + sentence.sentenceTxt.substring(start, end))
    s.toString
  }

  // if no true token is given, it is infered, but that could be slow in large scale
  def eventsAreTrue(tokenOnTrueSentence: Token, checkEventType: Boolean, checkEventTheme: Boolean, checkEventArguments: Boolean): Boolean = {
    val trueToken = if (tokenOnTrueSentence == null) this.sentence.trueSentence(this.sentence.indexOf(this)) else tokenOnTrueSentence
    val inferedEventOption = sentence.events.find((event: Event) => event.span.contains(this))
    val trueEventOption = sentence.trueSentence.events.find((event: Event) => event.span.contains(tokenOnTrueSentence))
    if (inferedEventOption.isDefined != trueEventOption.isDefined)
      false
    if (inferedEventOption.isDefined && trueEventOption.isDefined) {
      inferedEventOption.get.deepEqual(checkEventType, checkEventTheme, checkEventArguments, trueEventOption.get)
    }
    true
  }
}

/* He knows everything, but has to ask his members */
class Document(val id: String) extends VariableSeq[Sentence] {
  // for cross validation
  var inFoldIndex = -1
  // all tokens in the document
  def tokens = for (s <- this; t <- s) yield t
  // how does he present everything?
  def text: String = first.documentTxt

  var depTree: DependencyTree = null

  override def toString() = {
    val sr = new StringBuilder()
    val sentenceIter = this.elements
    while (sentenceIter.hasNext) {
      val s = sentenceIter.next
      sr ++= s + "\n"
      val tokenIter = s.elements
      while (tokenIter.hasNext) {
        val t = tokenIter.next
        sr ++= t + "--"
      }
      sr ++= "\n"
    }
    sr ++= "\n"
    sr.toString
  }
  // show all spans
  def spansToString(): String = {
    val sr = new StringBuilder()
    val sentenceIter = this.elements
    while (sentenceIter.hasNext) {
      val s = sentenceIter.next
      sr ++= s.spans + "\n"
    }
    sr.toString
  }
  // to lookup entities in this document (easier than to do it sentence wise, and the ids are unique for the whole document)
  var idLookup = new HashMap[String, Span]
  var geneLookup = new HashMap[String, Gene]
  var eventLookup = new HashMap[String, Event]
  val lengtha = this.length

  def reset(removeSpans: Boolean) = {
    for (sentence: Sentence <- this; event: Event <- sentence.events) {
      sentence.removeEvent(event, removeSpans)(null)
    }
    if (removeSpans) {
      for (sentence: Sentence <- this; (span: Span) <- sentence.spans; if (span.isOfEvent)) sentence.removeSpan(span)(null)
    }
    if (InitFile.initKeyValues.contains("resetTouchingWhileResetting") && InitFile.initKeyValues("resetTouchingWhileResetting") == "true") {
      for (sentence: Sentence <- this; trueEvent: Event <- sentence.trueSentence.events) trueEvent.touchedFromProposer = false
    }
  }

  // should be lazy val, eh? NOT for the frequency based stuff
  def mostImportantGene: String = {
    if (InitFile.miGeneFrequency) {
      // count all genes in document
      val c = new HashMap[String, Int]
      var mig: String = null
      for (s: Sentence <- this; g <- s.genes) {
        if (!c.contains(g.span.stringrepr)) c += g.span.stringrepr -> 0
        c += g.span.stringrepr -> (c(g.span.stringrepr) + 1)
        mig = g.span.stringrepr // just to have one to start with later
      }
      for (g <- c.keySet; if (c(g) > c(mig))) mig = g
      mig
    }
    else if (InitFile.miGeneFrequencyEvents) {
      // count all genes in document
      val c = new HashMap[String, Int]
      var mig: String = null
      val allGenesInArguments = for (s: Sentence <- this; e <- s.events; a <- e.arguments.filter(q => q.entity.isInstanceOf[Gene])) yield a.entity.asInstanceOf[Gene]
      for (g <- allGenesInArguments) {
        if (!c.contains(g.span.stringrepr)) c += g.span.stringrepr -> 0
        c += g.span.stringrepr -> (c(g.span.stringrepr) + 1)
        mig = g.span.stringrepr // just to have one to start with later
      }
      for (g <- c.keySet; if (c(g) > c(mig))) mig = g
      mig
    }
    else if (InitFile.miGeneFirst) {
      // all genes
      val allGenes = for (s: Sentence <- this; g <- s.genes) yield g
      val sortedGenes = Sorting.stableSort(allGenes, (g1: Gene, g2: Gene) => (g1.span.sentence.start < g2.span.sentence.start || (g1.span.sentence.start == g2.span.sentence.start && g1.span.start < g2.span.start)))
      sortedGenes.first.span.stringrepr
    }
    else if (InitFile.miGeneLast) {
      val allGenes = for (s: Sentence <- this; g <- s.genes) yield g
      val sortedGenes = Sorting.stableSort(allGenes, (g1: Gene, g2: Gene) => (g1.span.sentence.start < g2.span.sentence.start || (g1.span.sentence.start == g2.span.sentence.start && g1.span.start < g2.span.start)))
      sortedGenes.last.span.stringrepr
    }
    else if (InitFile.miGeneFirstEvent) {
      val allGenesInArgs = for (s: Sentence <- this; e <- s.events; a <- e.arguments; if (a.entity.isInstanceOf[Gene])) yield a.entity.asInstanceOf[Gene]
      val sortedGenes = Sorting.stableSort(allGenesInArgs, (g1: Gene, g2: Gene) => (g1.span.sentence.start < g2.span.sentence.start || (g1.span.sentence.start == g2.span.sentence.start && g1.span.start < g2.span.start)))
      if (sortedGenes.isEmpty) null else
        sortedGenes.first.span.stringrepr
    }
    else if (InitFile.miGeneLastEvent) {
      val allGenesInArgs = for (s: Sentence <- this; e <- s.events; a <- e.arguments; if (a.entity.isInstanceOf[Gene])) yield a.entity.asInstanceOf[Gene]
      val sortedGenes = Sorting.stableSort(allGenesInArgs, (g1: Gene, g2: Gene) => (g1.span.sentence.start < g2.span.sentence.start || (g1.span.sentence.start == g2.span.sentence.start && g1.span.start < g2.span.start)))
      if (sortedGenes.isEmpty) null else
        sortedGenes.last.span.stringrepr
    } else {
      null
    }
  }

  def triggerEventPairStrings =
    for(s:Sentence <- this ; e: Event <- s.trueSentence.events ; a:Argument <- e.arguments)
    yield a.entity.span.stringrepr + " --- " + e.clue.stringrepr + " --- " + a.entity.span.start + "+" + e.clue.start


}

/* A Gene is only defined by a span over tokens, that's it. No normalization today. */
class Gene(span: Span) extends Entity(span: Span) {
  override def toString(): String = {
    span.id + ":" + span.start + ":" + span.length + ":" + span.map(_.word).mkString(" ")
  }

  def outputFormat: String = {
    var spanStartDocOffset = span.first.docOffsets._1
    var spanEndDocOffset = Int.MinValue
    for (token <- span) spanEndDocOffset = token.docOffsets._2
    //System.err.println(">>"+span.realtext)
    val entityString = if (span.realtext != null) span.realtext else span.sentence.getDocumentText.substring(spanStartDocOffset, spanEndDocOffset) // the use of realtext leads to inconsistencies in offsets!
    span.id + "\tProtein " + spanStartDocOffset + " " + spanEndDocOffset + "\t" + entityString + "\n"
  }
}

/* An event is defined as a span of tokens, it has an eventtype and additional arguments. */
// for second part in a2, points to entities
class Event(var id: String, val clue: Span, val eventType: EventType)(implicit d: DiffList)
        extends Entity(clue: Span) with Variable {
  var touchedFromEvaluation = false // debug: only for use in truesentence
  var touchedFromProposer = false // debug: only for use in truesentence

  var isFull = false

  if (d != null) {
    val diff = new NewEventDiff
    diff.redo
    d += diff
    d += FlagDiff(clue.sentence)
  }

  val arguments = new ArrayBuffer[Argument] // everything else from second part of a2

  override def toString(): String = {id + ":" + eventType.value + ":" + clue.start + ":" + clue.length + ":" + clue.map(_.word).mkString(" ")} // was clue and arguments but thats too long


  /**
   * Relational representation of clue position and type
   */
  def clueTypeRelation =
    (clue.mainClueToken.position, eventType.value)

  /**
   * Relational representation of clue and argument relations
   */
  def clueArgRelation = for (arg <- arguments) yield
    (clue.mainClueToken.position, arg.entity.span.head.position, arg.roleType.value)

  /**
   * Relational representation of argument pairs.
   */
  def argPairRelation = for (arg1 <- arguments; arg2 <- arguments;
                             if (arg1.entity.span.head.position < arg2.entity.span.head.position)) yield
    (arg1.entity.span.head.position, arg2.entity.span.head.position)

  def sentence = span.sentence

  def relationalEval = new Eval
  
  class Eval {
    val clueArgRelation = Event.this.clueArgRelation
    val argPairRelation = Event.this.argPairRelation
    val clueTypeRelation = Event.this.clueTypeRelation
    val clueTypeTP = if (span.sentence.trueClueRelation(clueTypeRelation)) 1.0 else 0.0
    val clueTypeFP = 1.0 - clueTypeTP
    val clueArgTP = clueArgRelation.filter(t=>sentence.trueClueArgRelation(t)).size.toDouble
    val clueArgFP = clueArgRelation.size - clueArgTP
    val argPairTP = argPairRelation.filter(t=>sentence.trueArgPairRelation(t)).size.toDouble
    val argPairFP = argPairRelation.size - argPairTP
  }



  // if given list notToCountEvents is non-null, it will include this associated true event after
  def isATrueEventInSentence(checkEventType: Boolean, checkEventTheme: Boolean, checkEventArguments: Boolean, notToCountEvents: HashSet[Event]): Boolean = {
    // val candidateEvents = clue.sentence.trueSentence.events // old
    val candidateEvents =
    if (InitFile.useHeadForEval) {
      for (span <- clue.sentence.trueSentence.spans;
           if (span.mainClueToken.position == clue.mainClueToken.position);
           event <- span.events) yield {
        //        println("*** the head of %s is %s and is also the head of event span %s".format(
        //          clue.stringrepr,clue.head.word,span.stringrepr))
        event
      }
    } else for (span <- clue.sentence.trueSentence.spansStartingAt(clue.start).filter(
      s => s.length == clue.length);
                event <- span.events) yield event // new and faster, hopefully
    val res = candidateEvents.find(
      x => x.deepEqual(checkEventType, checkEventTheme, checkEventArguments, this) && (notToCountEvents == null || !notToCountEvents.contains(x)))
    if (notToCountEvents != null && res.isDefined) notToCountEvents.addEntry(res.get)
    res.isDefined //&& isNotInList
  }

  def touchEventInTrueSentence(needsType: Boolean, needsTheme: Boolean, needsAllArguments: Boolean): Boolean = {
    // val candidateEvents = clue.sentence.trueSentence.events // old
    val candidateEvents = for (
      span <- clue.sentence.trueSentence.spansStartingAt(clue.start).filter(
        s => s.length == clue.length);
      event <- span.events) yield event // new and faster, hopefully
    val res = candidateEvents.find((x: Event) => x.deepEqual(needsType, needsTheme, needsAllArguments, this))
    if (res.isDefined) res.get.touchedFromProposer = true
    res.isDefined
  }

  def outputFormatT: String = {
    var spanStartDocOffset = clue.first.docOffsets._1
    var spanEndDocOffset = Int.MinValue
    for (token <- clue) spanEndDocOffset = token.docOffsets._2
    val entityString = clue.sentence.getDocumentText.substring(spanStartDocOffset, spanEndDocOffset)
    clue.id + "\t" + eventType.value + " " + spanStartDocOffset + " " + spanEndDocOffset + "\t" + entityString + "\n"
  }

  def outputFormatE: String = {
    val sb = new StringBuilder()
    sb ++= id + "\t" + eventType.value + ":" + clue.id + " "
    for (arg <- arguments) {
      sb ++= arg.outputFormat + " "
    }
    sb.toString.trim + "\n"
  }
  // compare two events
  def deepEqual(checkEventType: Boolean, checkEventTheme: Boolean, checkEventArguments: Boolean, other: Event): Boolean = {
    var result = false
    val a =
    if (InitFile.useHeadForEval)
      this.clue.mainClueToken.position == other.clue.mainClueToken.position
    else
      this.clue.start == other.clue.start && this.clue.length == other.clue.length


    // accepting if the proposed theme is in the gold, we do not need all, for that use checkEventArguments
    val eventArgumentThemeResult = if (!checkEventTheme) true else {
      val thisThemeOption = this.arguments.find(x => x.roleType.value == "Theme")
      if (thisThemeOption.isDefined) {
        val thisTheme: Argument = thisThemeOption.get
        other.arguments.exists((x: Argument) => x.roleType.value == "Theme" && x.entity.span.deepEqual(thisTheme.entity.span) && x.entity.deepEqual(thisTheme.entity)) // todo check that again
      } else
        false // if there is no theme it cannot be true
    }

    val eventArgumentResult = if (!checkEventArguments) true else {
      !other.arguments.find((x: Argument) => !x.isATrueArgumentInEvent(this, false)).isDefined && // if we cannot find an argument which is not true in the other event
              !this.arguments.find((x: Argument) => !x.isATrueArgumentInEvent(other, false)).isDefined // then everything is alright, we do not want that to be defined!
    }
    if (checkEventType)
      result = a && this.eventType.value == other.eventType.value && eventArgumentResult && eventArgumentThemeResult
    else
      result = a && eventArgumentResult && eventArgumentThemeResult
    //System.err.println(result+"\tfor comparing with parameters\t"+checkEventType+"\t"+checkEventArguments+"\t"+this+"\t"+other+"\t"+this.arguments+"\t"+other.arguments)
    result
  }

  def getThemes(): Seq[Argument] = {
    arguments.filter(a => a.roleType.value == "Theme")
  }

  def changeEventTypeTo(eventType: String)(implicit diff: DiffList) { // TODO test
    diff += FlagDiff(this)
    for (e <- this.clue.sentence.events; if (e.arguments.exists(a => a.entity == this))) {
      diff += FlagDiff(e) // check again, also for higher order?
    }
    this.eventType.set(eventType)(diff)
  }

  var present = true

  def delete(implicit d: DiffList) = {
    val diff = new DeleteEventDiff
    diff.redo
    if (d != null) d += diff
    if (d != null) d += new FlagDiff(clue.sentence)
  }

  case class AddArgumentDiff(arg: Argument) extends Diff {
    def undo = {
      Event.this.arguments -= arg
    }

    def redo = {
      Event.this.arguments += arg
    }

    def variable = Event.this
  }

  case class RemoveArgumentDiff(arg: Argument) extends Diff {
    def undo = {
      Event.this.arguments += arg
    }

    def redo = {
      Event.this.arguments -= arg
    }

    def variable = Event.this
  }

  case class FlagDiff(v: Variable) extends Diff {
    def undo = {}

    def redo = {}

    def variable = v
  }

  case class DeleteEventDiff() extends Diff {
    def undo = {
      present = true
    }

    def redo = {
      present = false
    }

    def variable = Event.this
  }

  case class NewEventDiff() extends Diff {
    def undo = {
      present = false
    }

    def redo = {
      present = true
    }

    def variable = Event.this
  }

  def addEntityAsArgument(entityAsArgument: Entity, role: String)(implicit d: DiffList): Argument = {
    //if (InitFile.dontAddDuplicates && arguments.contains((a:Argument)=>a.entity == entityAsArgument)) return null
    val roleType = new RoleType // should diff itself
    roleType.set(role)(d) //  diffs itself
    val arg = new Argument("ARG0", entityAsArgument, roleType, this)(d)
    val diff = new AddArgumentDiff(arg: Argument)
    diff.redo // really adds the argument
    if (d != null) d += diff // to allow undo and another redo
    //d += new FlagDiff(Event.this) // add event to difflist, not needed, as AddArgumentDiff uses the event as variable
    if (d != null) d += new FlagDiff(entityAsArgument.span.sentence)
    arg // returning the argument
  }

  def removeEntityAsArgument(entityAsArgument: Argument)(implicit d: DiffList): Argument = {
    if (!entityAsArgument.eventWithThisArgument.equals(this)) throw new Exception("Something went wrong, you tried to remove an argument from the wrong event, that should not be possible!")
    entityAsArgument.delete(d) // sets present to false, puts the argument into difflist
    val diff = new RemoveArgumentDiff(entityAsArgument)
    diff.redo // really removes the argument
    if (d != null) d += diff // to allow undo and another redo
    //d += new FlagDiff(Event.this) // add event to difflist, not needed, as RemoveArgumentDiff uses the event as variable
    if (d != null) d += new FlagDiff(entityAsArgument.entity.span.sentence) // null could be while reading the files first time
    entityAsArgument // but it is not existing any more!
  }

  // should not be called on true sentences, it finds it own true events in them, returns (numOfMissedArguments,InNumOfTrueEventCandidates)
  def numberOfMissedArguments: (Double, Double) = {
    if (clue.sentence.isInstanceOf[TrueSentence]) throw new Exception("NumberOfMissedArguments should only be called on normal sentences!")
    var missedArguments = 0.0
    // find list of true event candidates (using the clue and event type)
    val fittingTrueEvents = for ((someTrueEvent: Event) <- clue.sentence.trueSentence.events; if (someTrueEvent.deepEqual(true, false, false, this))) yield someTrueEvent
    // count how many arguments from the fittingTrueEvents are not in this event
    for ((fittingTrueEvent: Event) <- fittingTrueEvents; trueArg <- fittingTrueEvent.arguments; if (!this.arguments.find(arg => trueArg.deepEqual(arg)).isDefined))
      missedArguments += 1.0
    //System.err.println("SHOULD BE GREATER THAN ZERO: "+(missedArguments,fittingTrueEvents.length))
    (missedArguments, fittingTrueEvents.length)
  }

  def evalFMeasureOfArguments(): (Double, Double, Double, Double, Double, Double) = {
    evalFMeasureOfArguments(null)
  }

  def evalFMeasureOfArguments(stream: PrintStream): (Double, Double, Double, Double, Double, Double) = {
    var tp, fp, fn = 0.0
    // over all arguments in me
    for (arg <- arguments) {
      if (arg.present) {
        if (arg.isATrueArgumentInEvent(this, true))
          tp += 1.0
        else
          fp += 1.0
      }
    }
    // did we miss an argument in me
    // find list of possibly correct events (check the clue and type)
    val numOfMissedArg = this.numberOfMissedArguments
    fn = numOfMissedArg._1 // / numOfMissedArg._2 ??

    val prec = if ((tp + fp) == 0) 1.0 else tp / (tp + fp)
    val rec = if ((tp + fn) == 0) 0.0 else tp / (tp + fn)
    val f1 = if ((prec + rec) == 0) 0.0 else 2 * (prec * rec) / (prec + rec)
    if (stream != null)
      stream.println(tp + "\t" + fp + "\t" + fn + "\t" + prec + "\t" + rec + "\t" + f1)
    (tp, fp, fn, prec, rec, f1)
  }

  def containsArgument(arg: Argument): Boolean = {
    arguments.exists((other: Argument) => arg.deepEqual(other))
  }

  def containsEntityAsArgumentNonDeep(e: Entity): Boolean = {
    arguments.exists((other: Argument) => e == other.entity)
  }

  def uniqueRepresentation: String = {
    // indexes of this span, sentence-id and doc-id
    val s = clue.sentence
    val sortedArguments = Sorting.stableSort(arguments, (k1: Argument, k2: Argument) => k1.entity.span.start < k2.entity.span.start)
    val r = s.doc.id + "; " + s.sentenceNumInDoc + "; " + eventType.value + ":" + clue.start + ":" + clue.length + "->" + sortedArguments.map(x => x.roleType.value + ":" + x.entity.span.start + ":" + x.entity.span.length + ":" + (if (x.isInstanceOf[Event]) x.asInstanceOf[Event].hash else "Gene")).mkString(";")
    //System.err.println("FEATURE:"+r)
    r
  }

  def hash: String = uniqueRepresentation

}

case class FlagDiff(v: Variable) extends Diff {
  def undo = {}

  def redo = {}

  def variable = v
}

class Argument(val id: String, var entity: Entity, val roleType: RoleType, val eventWithThisArgument: Event)(implicit d: DiffList) extends Variable {
  def hash: String = {
    entity.span.sentence.doc.id + "; " + entity.span.sentence.sentenceNumInDoc + "; " + "->" + roleType.value + ":" + entity.span.start + ":" + entity.span.length + ":" + (if (entity.isInstanceOf[Event]) entity.asInstanceOf[Event].hash else "Gene")
  }
  //val parentEvents : Seq[Event] = mention.entities.filter(x => x.isInstanceOf[Event]).map(x => x.asInstanceOf[Event]) // makes not too much sense
  override def toString(): String = id + ":" + entity + ":" + roleType.value

  def outputFormat: String = {
    if (entity.isInstanceOf[Event])
      roleType.value + ":" + entity.asInstanceOf[Event].id
    else if (entity.isInstanceOf[Gene])
      roleType.value + ":" + entity.span.id
    else
      "THERE IS SOMETHING WRONG WHAT CAN NEVER BE FIXED!"
  }

  // check, if this argument belongs to the given event, check span
  def isATrueArgumentInEvent(event: Event, searchForEquivalentTrueEventToCompare: Boolean): Boolean = {
    if (!this.entity.span.sentence.isInstanceOf[TrueSentence] && !this.present) {
      System.err.println("First test: " + this.entity.span.sentence.isInstanceOf[TrueSentence])
      System.err.println("Second test: " + this.present)
      throw new Exception("This argument does not exist anymore in this variable sentence!")
    }
    if (this.entity.span.sentence.isInstanceOf[TrueSentence] && searchForEquivalentTrueEventToCompare) throw new Exception("It makes no sense to try to evaluate a true event when asked to search for it!")
    // find a fitting true event in the true sentence
    val trueEventCandidate = {
      if (searchForEquivalentTrueEventToCompare)
        {
          val candidateOpt = entity.span.sentence.trueSentence.events.find((e: Event) => e.deepEqual(true, false, false, event)) // never a true at 2nd position: recursion
          if (candidateOpt.isDefined) candidateOpt.get else null
        }
      else
        event
    }
    if (trueEventCandidate != null)
      trueEventCandidate.containsArgument(this)
    else
      false
  }

  /* Compares this argument with another by means of included span, gene or event class and if the latter, event type, NOT arguments */
  def deepEqual(other: Argument): Boolean = {
    other.entity.deepEqual(this.entity) && other.roleType.value == this.roleType.value
  }

  var present = false

  // from here it comes more or less from event, copycat
  val diff = new NewArgumentDiff
  diff.redo
  if (d != null) d += diff

  def delete(implicit d: DiffList) = {
    val diff = new DeleteArgumentDiff
    diff.redo
    if (d != null) d += diff
  }

  case class DeleteArgumentDiff() extends Diff {
    def undo = {
      present = true
    }

    def redo = {
      present = false
    }

    def variable = Argument.this
  }

  case class NewArgumentDiff() extends Diff {
    def undo = {
      present = false
    }

    def redo = {
      present = true
    }

    def variable = Argument.this
  }
}



object Data {
  var checkEntityHomogenityOnSpans = false
  // for nice printing
  val df = new DecimalFormat("0.00")
  // to make a deep copy
  def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A =
    Marshal.load[A](Marshal.dump(a))
  // possible types of events
  val possibleEventTypes = new Array[String](10)
  possibleEventTypes(0) = "Binding"
  possibleEventTypes(1) = "Gene_expression"
  possibleEventTypes(2) = "Localization"
  possibleEventTypes(3) = "Negative_regulation" // can have events as arguments
  possibleEventTypes(4) = "Phosphorylation"
  possibleEventTypes(5) = "Positive_regulation" // can have events as arguments
  possibleEventTypes(6) = "Protein_catabolism"
  possibleEventTypes(7) = "Regulation" // can have events as arguments
  possibleEventTypes(8) = "Transcription"
  possibleEventTypes(9) = "NOTYPE"
  // no Entity, Equiv, Negation, Speculation

  val possibleArgTypes = new Array[String](2)
  possibleArgTypes(0) = "Theme"
  possibleArgTypes(1) = "Cause"

  val READMSG = false

  val stemmer = new EnglishStemmer

  def stem(s: String): String = {
    stemmer.setCurrent(s);
    stemmer.stem();
    stemmer.stem();
    stemmer.stem();
    stemmer.stem();
    stemmer.getCurrent();
  }

  val cluePerTypeGazateer = new HashMap[String, HashSet[String]]
  val clueGazateer = new HashSet[String]
  val clueTokenGazateer = new HashSet[String]

  /* Read one instance from prefix.txt, id.a1, and id.a2 */
  def readOneFile(prefix: String): Document = readOneFile(prefix,false) 
  def readOneFile(prefix: String, guessEquivalent:Boolean): Document = {
    // file we need
    val slash = prefix.lastIndexOf("/")
    val id = prefix.substring(slash + 1, prefix.length)
    val path = prefix.substring(0, slash + 1)
    val txtfile = id + ".txt"
    val a1file = id + ".a1"
    val a2file = id + ".a2.t1"
    System.err.println("Reading from " + path + "{" + txtfile + "," + a1file + "," + a2file + "}");
    // read txt, removing line breaks, tabs, cr
    val fis = new FileInputStream(path + txtfile)
    val bis = new BufferedInputStream(fis)
    val source = Source.fromInputStream(bis)
    val docTxt = source.collect.mkString.replaceAll("[\n\t\r ]", " ");
    bis.close
    fis.close
    val si = java.text.BreakIterator.getSentenceInstance
    si.setText(docTxt)

    // the whole document where everything is stored in
    var doc = new Document(id)

    var l = 0;
    var b = -2
    var sentenceNum = 0
    while (b != -1) {
      b = si.next(1)
      if (b != -1) {
        val sentenceTxt = docTxt.substring(l, b)
        var sentence = new Sentence(l, b, docTxt, sentenceNum, doc) // automatically generates its true sentence
        doc += sentence
        l = b
        sentenceNum += 1
      }
    }
    addGenes(path + a1file, doc)
    if (new File(path + a2file).exists) addEvents(path + a2file, doc)
    if (guessEquivalent) {
      for(s <- doc) guessEquivalentGenesAndRemove(s.trueSentence)
    }
    else if (new File(path + a2file).exists)
      filterEquivalentEntities(path + a2file, doc)
    doc
  }


  // add genes to sentence and true sentence
  def addGenes(annotFileName: String, doc: Document) {
    val allowDataBasedRetokenization = false
    if (allowDataBasedRetokenization) { // do a finer retokenization with the data
      val fis = new FileInputStream(annotFileName)
      val bis = new BufferedInputStream(fis)
      val source = Source.fromInputStream(bis)
      val a1lines = source.getLines
      for (val l <- a1lines) {
        System.err.println("Searching for: " + l)
        val line = l.replaceAll("[\n\r]", "")
        val cols = line split "[ \t]"
        if (cols.length > 4) { // well formed?
          val id = cols(0)
          val start = cols(2)
          val end = cols(3)
          // to which sentence does this entity belong?
          for (sentence <- doc; if (sentence.offsets._1 <= start.toInt); if (sentence.offsets._2 >= end.toInt)) { // only once went in
            System.err.println("Searching in sentence: " + sentence)
            val entityInSentenceStart = start.toInt - sentence.offsets._1
            val entityInSentenceEnd = end.toInt - sentence.offsets._1
            val x = sentence.sentenceTxt.substring(entityInSentenceStart, entityInSentenceEnd)
            // and which tokens belong to this entity?
            for (token <- sentence; // use this token?
                 if (token.offsets._1 <= entityInSentenceStart && token.offsets._2 >= entityInSentenceEnd)
            ) { // YES, use this token
              // to the token split!
            }
          }
        }
      }
      bis.close
      fis.close
    }
    // Add a1 annotations (Genes and Proteins) to sentences
    val fis2 = new FileInputStream(annotFileName)
    val bis2 = new BufferedInputStream(fis2)
    val source2 = Source.fromInputStream(bis2)
    val a1lines = source2.getLines
    // overall gene/protein mentions
    for (val l <- a1lines) {
      //			System.err.println("Searching for: "+l)
      val line = l.replaceAll("[\n\r]", "")
      val cols = line split "[ \t]"
      if (cols.length > 4) { // well formed?
        val id = cols(0)
        val start = cols(2)
        val end = cols(3)
        // to which sentence does this entity belong?
        for (sentence <- doc; if (sentence.offsets._1 <= start.toInt); if (sentence.offsets._2 >= end.toInt)) { // only once went in
          //					System.err.println("Searching in sentence: "+sentence)
          val entityInSentenceStart = start.toInt - sentence.offsets._1
          val entityInSentenceEnd = end.toInt - sentence.offsets._1
          val x = sentence.sentenceTxt.substring(entityInSentenceStart, entityInSentenceEnd)
          var tokenNumStart = Int.MinValue
          var tokenNumLength = Int.MinValue
          // and which tokens belong to this entity?
          // test
          //       for (token <- sentence)
          //         {
          //           System.err.println(token.getText)
          //           System.err.println(token.offsets._1 +" >= "+entityInSentenceStart +" = " + (token.offsets._1 >= entityInSentenceStart))
          //           System.err.println(token.offsets._2 +" <= "+entityInSentenceEnd +" = " + (token.offsets._2 <= entityInSentenceEnd))
          //           System.err.println("-------------")
          //         }
          // testend
          for (token <- sentence; // use this token?
               if (token.offsets._1 >= entityInSentenceStart && token.offsets._2 <= entityInSentenceEnd)
          ) { // YES, use this token
            if (tokenNumStart == Int.MinValue) tokenNumStart = sentence.indexOf(token);
            tokenNumLength = sentence.indexOf(token) - tokenNumStart + 1 // minimum length is 1
          }
          // for the weird cases: do a string based first match search, find parts of an entity
          var approxSearched = false
          if (tokenNumStart == Int.MinValue && tokenNumLength == Int.MinValue) {
            System.err.println("Found a weird case in sentence " + sentence + ": " + l)
            // search the closest token matching
            val matchingList = sentence.filter((x: Token) => (x.word.indexOf(cols.last) != -1))
            // remember to set the real text later
            approxSearched = true
            //System.err.println(matchingList)
            var matchingTok: Token = matchingList(0) // init for search for closest
            //System.err.println(matchingTok.docOffsets._1 +" - "+ start)
            for (candidate <- matchingList) if (Math.abs(start.toInt - matchingTok.docOffsets._1) > Math.abs(start.toInt - candidate.docOffsets._1)) matchingTok = candidate

            //val matching = sentence.find((x:Token) => (x.word.indexOf(cols.last) != -1) )
            //System.err.println("Using "+matching.get)
            System.err.println("Using closest " + matchingTok)
            tokenNumStart = sentence.indexOf(matchingTok);
            tokenNumLength = sentence.indexOf(matchingTok) - tokenNumStart + 1
          }
          // now we have the start and end of this entity on token number level
          //					System.err.println(tokenNumStart+":"+tokenNumLength+"-S"+sentence.length)
          val textToSet = if (approxSearched) cols.last else null
          var geneMention = new Span(sentence, tokenNumStart, tokenNumLength, null, id, textToSet)(null)
          val geneA = new Gene(geneMention)
          geneMention.addEntity(geneA)(null)
          var geneMentionForTrueSentence = new Span(sentence.trueSentence, tokenNumStart, tokenNumLength, null, id, textToSet)(null)
          val geneB = new Gene(geneMentionForTrueSentence)
          geneMentionForTrueSentence.addEntity(geneB)(null)
          doc.geneLookup.put(id, geneB) // changed that recently from geneA, ok?
          if (READMSG) System.err.println("Read Gene " + id + ":" + geneMention)
          // test, yes spans are in, but not really the genes...
          //					for(x <- s.spans) err.println(x)
        }
      }
    } // end over file
    bis2.close
    fis2.close
  }

  // if there equivalent entities, keep the shortest ones or longest ones
  def filterEquivalentEntities(annotFileName: String, doc: Document) {
    //System.err.println(annotFileName)
    if (InitFile.filterforequivalent) {
      val fis = new FileInputStream(annotFileName)
      val bis = new BufferedInputStream(fis)
      val source = Source.fromInputStream(bis)
      val lines = source.getLines
      // debug
//      for(s <- doc) {
//        guessEquivalentGenesAndRemove(s.trueSentence)
//      }
      //
      // for each line specifying an equivalent, keep only one of them
      for ((l: String) <- lines; if (l.startsWith("*\tEquiv"))) {
        val equivTs: Array[String] = l.trim.substring(8).split(' ')
        val equivTsList = equivTs.toList
        // get the one we want to keep by sorting
        val sortedTs = if (InitFile.useshortest)
          Sorting.stableSort(equivTsList, (a1: String, a2: String) => doc.geneLookup(a1).span.stringrepr.length < doc.geneLookup(a2).span.stringrepr.length)
        else
          Sorting.stableSort(equivTsList, (a1: String, a2: String) => doc.geneLookup(a1).span.stringrepr.length > doc.geneLookup(a2).span.stringrepr.length)
        // remove all but the first
        val keptGene = doc.geneLookup(sortedTs(0))
        for (toremove <- sortedTs; if (toremove != sortedTs(0))) { // all are removed from the list but the first, this is the one which should be used, therefore it should be used as arguments in events instead of the removed ones
          val entity: Gene = doc.geneLookup(toremove)
          // for debugging, find an event which uses this gene
          val criticalEvents = for (s <- doc; event <- s.trueSentence.events; if (event.arguments.exists(x => x.entity == entity))) yield event
          //criticalEvents.foreach(x => {System.err.println("Critical: " + x.outputFormatE.trim + " " + x.outputFormatT.trim); x.arguments.foreach(y => System.err.println("\t" + y.entity.genericOutputFormatTorE.trim))})
          // removing span but keep the entity, set the new span to that entity
          entity.span.sentence.removeSpan(entity.span)(null)
          val spanOnVariableSentence = entity.span.sentence.asInstanceOf[TrueSentence].variableSentence.spansStartingAt(entity.span.start).find(s => s.length == entity.span.length).get
          entity.span.sentence.asInstanceOf[TrueSentence].variableSentence.removeSpan(spanOnVariableSentence)(null)
          System.err.println("Removing " + toremove + " (" + entity.span.stringrepr + ") kept equivalent " + sortedTs(0) + " (" + keptGene.span.stringrepr + ")")
          entity.span = keptGene.span
          // replace argument entities
          for (e <- criticalEvents; a: Argument <- e.arguments) if (a.entity == entity) a.entity = keptGene // new
          //criticalEvents.foreach(x => {System.err.println("Critical: " + x.outputFormatE.trim + " " + x.outputFormatT.trim); x.arguments.foreach(y => System.err.println("\t" + y.entity.genericOutputFormatTorE.trim))})
          //System.err.println("------------------------------------\n")
        }
      }
      bis.close
      fis.close
    }
  }

  def guessEquivalentGenesAndRemove(s:TrueSentence) : ArrayBuffer[Gene] = {
    // more simple then running over succeeding genes, run over pairs with a max distance
    for(g1:Gene <- s.genes ; g2:Gene <- s.genes ; if (g1 != g2 && g2.span.start > g1.span.end && (g2.span.start - g1.span.end < 3))) {
      if (g2.span.start > 1 && g2.span.end < s.length-1 && s(g2.span.start-1).word=="(" && s(g2.span.end+1).word==")") {
        val (toRemoveGene,keptGene) = if (InitFile.useshortest) {
          if (g2.span.stringrepr.length > g1.span.stringrepr.length) (g2,g1) else (g1,g2)
        } else {
          if (g2.span.stringrepr.length < g1.span.stringrepr.length) (g2,g1) else (g1,g2)
        }
        val criticalEvents = for (event <- s.events; if (event.arguments.exists(x => x.entity == toRemoveGene))) yield event
        toRemoveGene.span.sentence.removeSpan(toRemoveGene.span)(null)
        val spanOnVariableSentence = toRemoveGene.span.sentence.asInstanceOf[TrueSentence].variableSentence.spansStartingAt(toRemoveGene.span.start).find(s => s.length == toRemoveGene.span.length).get
        toRemoveGene.span.sentence.asInstanceOf[TrueSentence].variableSentence.removeSpan(spanOnVariableSentence)(null)
        System.err.println("Removing Guess " + toRemoveGene.span.stringrepr + " kept equivalent " + keptGene.span.stringrepr)
        toRemoveGene.span = keptGene.span
        // replace argument entities
        for (e <- criticalEvents; a: Argument <- e.arguments) if (a.entity == toRemoveGene) a.entity = keptGene
      }
    }
    new ArrayBuffer[Gene]
  }

  def isSymbolBetweenTokens(symbol: String, t1: Token, t2: Token): Boolean = {
    val s = t1.sentence
    if (symbol == ";") return s.semiCount(t1) != s.semiCount(t2)
    val (pos1, pos2) = if (t1.sentencePos < t2.sentencePos) (t1.sentencePos, t2.sentencePos) else (t2.sentencePos, t1.sentencePos)
    val found = s.exists((t: Token) => (t.sentencePos > pos1 && t.sentencePos < pos2 && t.word == symbol))
    //    if (found && symbol == ";" && s.first.word == "Subsequent")
    //      System.err.println("Found %s in sentence %s".format(symbol,s.map(_.word).mkString(" ")))
    found
  }

  // add only to the true sentence... let's see what happens with genes...
  def addEvents(annotFileName: String, doc: Document) {
    // Add a1 annotations (Genes and Proteins) to sentences,
    val fis = new FileInputStream(annotFileName)
    val bis = new BufferedInputStream(fis)
    val source = Source.fromInputStream(bis)
    val lines = source.getLines
    // first pass, collect all Spans and Events
    for ((l: String) <- lines; if (!l.split("[ \t]")(1).equals("Entity")) && (!InitFile.ignoreregulations || (!l.split("[ \t]")(1).contains("egulation")))) {
      //		  System.err.println("RK>"+l)
      val line = l.replaceAll("[\n\r]", "").replaceAll(" +", " ")
      //			   System.err.println("RK>"+line)
      val cols = line split "[ \t]"
      if (line.startsWith("T") && cols.length > 4) { // well formed to be an argument?
        val id = cols(0)
        val start = cols(2)
        val end = cols(3)

        // if is a theme, it has offsets as an entity and needs to be added to a sentence first
        // we assume that all possible arguments are prior to the events in the file. This holds for the sample data,
        // to which sentence does this argument belong?
        for (s <- doc; if (s.offsets._1 <= start.toInt); if (s.offsets._2 >= end.toInt)) { // only once went in
          val entityInSentenceStart = start.toInt - s.offsets._1
          val entityInSentenceEnd = end.toInt - s.offsets._1
          val x = s.sentenceTxt.substring(entityInSentenceStart, entityInSentenceEnd)
          var tokenNumStart = Int.MinValue
          var tokenNumLength = Int.MinValue
          // and which tokens belong to this argument?
          for (t <- s; // use this token?
               if (t.offsets._1 >= entityInSentenceStart && t.offsets._2 <= entityInSentenceEnd)
          ) { // YES: seq.length
            if (tokenNumStart == Int.MinValue) tokenNumStart = s.indexOf(t);
            tokenNumLength = s.indexOf(t) - tokenNumStart + 1 // minimum length is 1
          }
          // for the weird cases: do a string based first match search, find parts of an entity
          if (tokenNumStart == Int.MinValue && tokenNumLength == Int.MinValue) {
            System.err.println("Found a weird case in sentence " + s + ": " + l)
            val matching = s.find((x: Token) => (x.word.indexOf(cols.last) != -1))
            System.err.println("Using " + matching.get)
            tokenNumStart = s.indexOf(matching.get);
            tokenNumLength = s.indexOf(matching.get) - tokenNumStart + 1
          }

          // now we have the start and end of this entity on token number level
          //System.err.println(s.trueSentence.toString(true)+"\t"+tokenNumStart+"\t"+tokenNumLength)
          var span = new Span(s.trueSentence, tokenNumStart, tokenNumLength, null, id)(null) // event for that span needs to be set later
          doc.idLookup.put(id, span)
          if (READMSG) System.err.println("Read " + id + ":" + span)
        }
      }
      else if (line.startsWith("E") && cols.length > 0) { // only the event itself, not the stuff it includes, this will be done in second pass
        val id = cols(0)
        val eventDeclaration = cols(1).split(" ")
        val eventType = eventDeclaration(0)
        val eventClueString = eventType.split(":")(0)
        val eventClueId = eventType.split(":")(1)
        /* build the event */
        // get the argument which is the clue
        val span = doc.idLookup(eventClueId)
        // for building the event, we only need the type and the span
        val etype = new EventType
        etype.set(eventClueString)(null)
        var event = new Event(id, span, etype)(null)
        //span.sentence.addEvent(event)
        span.addEntity(event)(null)
        // for caching, span is on a truesentence
        span.sentence.asInstanceOf[TrueSentence].cachedEvents += event
        doc.eventLookup.put(id, event)
        if (READMSG) System.err.println("Read " + id + ":" + span)
      }
    }
    bis.close
    fis.close

    // second pass, connect events to everything else
    val fis2 = new FileInputStream(annotFileName)
    val bis2 = new BufferedInputStream(fis2)
    val source2 = Source.fromInputStream(bis2)
    val lines2ndpass = source2.getLines
    for (l <- lines2ndpass; if ((!InitFile.ignoreregulations) || (!l.split("[ \t]")(1).contains("egulation")))) {
      val line = l.replaceAll("[\n\r]", "").replaceAll(" +", " ")
      //   			err.println(">>"+line)
      val cols = line split "[\t]"
      if (line.startsWith("E") && cols.length > 0) {
        //  			         System.err.println("Line: #"+line+"#")
        val id = cols(0)
        val eventDeclaration = cols(1).split(" ") // in here is, what we need to add to the event (by pointers)
        // get the current event object, to which stuff is added
        val currentEvent = doc.eventLookup(id)
        if (READMSG) System.err.println("Completing " + currentEvent + " (of class Event: " + (currentEvent.isInstanceOf[Event]) + ")")
        // what should we add?
        //  				err.println(cols(1))
        for (i <- 1 to eventDeclaration.length - 1) {
          val eventDeclarationSplit = eventDeclaration(i).split(":")
          //  					System.err.println("Splitting "+eventDeclaration(i))
          val role = new RoleType
          val roleString = eventDeclarationSplit(0).replaceAll("Theme[0-9]*", "Theme").replaceAll("Site[0-9]*", "Site")
          //if (!eventDeclarationSplit(0).equals(roleString)) System.err.println(eventDeclarationSplit(0)+"---"+roleString)
          role.set(roleString)(null)
          val entityForArgument: Entity = if (eventDeclarationSplit(1).startsWith("T"))
            if (doc.geneLookup.contains(eventDeclarationSplit(1))) doc.geneLookup(eventDeclarationSplit(1)) else null // in task 2, we get a null back, Entities are not recognized now
          else if (doc.eventLookup.contains(eventDeclarationSplit(1))) // could be false because of removing entities which span over sentences 
            doc.eventLookup(eventDeclarationSplit(1))
          else {
            System.err.println("Did not find entity for argument: " + eventDeclarationSplit(1))
            null
          }
          if (entityForArgument != null) {
            if (entityForArgument.span.sentence == currentEvent.clue.sentence) {
              if (READMSG) System.err.println("    Adding " + i + ": " + role + "---" + entityForArgument)
              val argument = new Argument(eventDeclarationSplit(1), entityForArgument, role, currentEvent.asInstanceOf[Event])(null)
              currentEvent.asInstanceOf[Event].arguments += argument
              //              if (currentEvent.clue.start == argument.entity.span.start) // debug for selfregulation
              //                System.err.println("DEBUG1: "+currentEvent.outputFormatE.trim+"   "+currentEvent.outputFormatT.trim+"   "+argument.entity.genericOutputFormatT.trim)
            } else if (InitFile.removeSentenceBreakingEvents) {
              System.err.println("Skipping event " + currentEvent + " because of argument entity " + entityForArgument + "! They are not in the same sentence!")
              currentEvent.clue.sentence.removeEvent(currentEvent, true)(null) // remove from sentence-span structure
              currentEvent.clue.sentence.asInstanceOf[TrueSentence].cachedEvents -= currentEvent // remove from true sentence cash
              doc.eventLookup.removeKey(currentEvent.id) // remove from lookup table
              // remove this event from all other events where it was an argument already
              for (event: Event <- currentEvent.clue.sentence.events; arg: Argument <- event.arguments; if (arg.entity == currentEvent)) event.removeEntityAsArgument(arg)(null)
            }
          }
        }
      }
    }
    bis2.close
    fis2.close
  }

  def setIDsInDocument(doc: Document) {
    // get maximal T-id for gene, then we continue there
    var maxGeneTid = -1
    for ((s: Sentence) <- doc; (gene: Gene) <- s.genes)
      if (maxGeneTid < gene.span.id.substring(1).toInt)
        maxGeneTid = gene.span.id.substring(1).toInt
    var currentEventSpanID = maxGeneTid + 1
    // set all event-span ids
    for ((s: Sentence) <- doc; (e: Event) <- s.events) {
      e.span.id = "T" + currentEventSpanID
      currentEventSpanID += 1
    }
    // set all event ids, these can just be counted
    var currentEventID = 1
    for ((s: Sentence) <- doc; (e: Event) <- s.events) {
      e.id = "E" + currentEventID
      currentEventID += 1
    }
  }

  def writeDocumentToFiles(doc: Document, txtFileName: String, a1FileName: String, a2FileName: String, writeTrueSentences: Boolean) = {
    // set id's while writing the stuff, not before!
    var maxGeneTid = -1
    for ((s: Sentence) <- doc; (gene: Gene) <- s.genes)
      if (maxGeneTid < gene.span.id.substring(1).toInt)
        maxGeneTid = gene.span.id.substring(1).toInt
    var currentEventSpanID = maxGeneTid + 1

    // concatenate all sentences and write to text file. We mainly ignore that originally the title was in an extra line
    // if txtFileName is null, we do not write
    val sb = new StringBuilder()
    for (sentence <- doc) {
      val text = sentence.getText.replaceAll("  $", " \n") // ok, we put some line breaks in
      sb ++= text
    }
    val docTxt = sb.toString
    if (txtFileName != null) {
      val outputstream = new java.io.FileWriter(txtFileName)
      outputstream.write(docTxt)
      outputstream.close
    }



    var a2Outputstream = new java.io.FileWriter(a2FileName)
    val estring = new StringBuilder

    // get all spans on the sentences which are genes and write them
    var a1Outputstream = new java.io.FileWriter(a1FileName)
    for (sentence <- doc) {
      val toUseSentence: Sentence = if (writeTrueSentences) sentence.trueSentence else sentence
      for (gene: Gene <- toUseSentence.genes) {
        a1Outputstream.write(gene.outputFormat)
      }
    }

    // set all E ids
    var currentEventID = 1
    for ((s: Sentence) <- doc; (e: Event) <- s.events) {
      e.id = "E" + currentEventID
      currentEventID += 1
    }
    // write all events, we only need to care about the clue Ts, as they will never be used as something else then the clue, otherwise E is used
    for (sentence <- doc) {
      val toUseSentence: Sentence = if (writeTrueSentences) sentence.trueSentence else sentence
      val alreadyWrittenPerTypeAndId = new HashMap[(String, Span), String]
      for (event: Event <- toUseSentence.events) {
        if (!alreadyWrittenPerTypeAndId.contains((event.eventType.value, event.clue))) {
          event.clue.id = "T" + currentEventSpanID
          currentEventSpanID += 1
          a2Outputstream.write(event.outputFormatT)
          alreadyWrittenPerTypeAndId += (event.eventType.value, event.clue) -> event.clue.id
        } else { // revisiting a T, was already written, only resetting to the correct id
          event.clue.id = alreadyWrittenPerTypeAndId((event.eventType.value, event.clue))
        }
        estring ++= (event.outputFormatE)
      }
    }

    // print all arguments for the events
    a2Outputstream.write(estring.toString)
    a1Outputstream.close
    a2Outputstream.close
  }

  def readCorpus(filePrefix: String): Array[Document] = readCorpus(filePrefix,false)

  def readCorpus(filePrefix: String,guessEquivalent:Boolean): Array[Document] = {
    val corpus = new ArrayBuffer[Document]
    for (file <- new File(filePrefix).listFiles; if (file.getName().endsWith(".txt"))) {
      val id = file.getName().substring(0, file.getName().lastIndexOf(".txt"))
      val doc = readOneFile(filePrefix + "/" + id, guessEquivalent)
      if (InitFile.selftokenized)
        StanfordDependencies.attachToDoc(filePrefix + "/sd/" + id + ".txt", doc)
      else {
        StanfordDependencies.resetTrees
        StanfordDependencies.attachToDocWithReToc(filePrefix + "/mc-sd/" + id + ".txt", doc,
          StanfordDependencies.tokenToDepForward, StanfordDependencies.tokenToDepBackward)
      } //
      //      if (InitFile.mc) StanfordDependencies.attachToDocWithReToc(filePrefix + "/mc-sd/" + id + ".txt", doc)
      if (InitFile.fixTrees) StanfordDependencies.fixTrees(doc)
      StanfordDependencies.generateSDPath(doc)
      ///
      POSReader.attachToDoc(filePrefix + "/ptree/" + id + ".txt", doc)
      // 			doc(0).foreach(err.println)
      corpus += doc
    }
    corpus.toList.sort((d1, d2) => d1.id.compareTo(d2.id) >= 0).toArray
  }

  /* Evaluates whole Corpus, prints stats and returns F */
  def evaluateCorpusOnEvents(prefix: String, iteration: Int, corpus: Array[Document], evalEventType: Boolean, evalEventTheme: Boolean, evalEventArgument: Boolean): Double = {
    var tp, fp, fn = 0.0
    for (doc <- corpus; (sentence: Sentence) <- doc) {
      val (atp, afp, afn, aprec, arec, af1) = sentence.eval(evalEventType, evalEventTheme, evalEventArgument, null, false, false, false)
      tp += atp
      fp += afp
      fn += afn
    }
    val prec = if ((tp + fp) == 0) 1.0 else tp / (tp + fp)
    val rec = if ((tp + fn) == 0) 0.0 else tp / (tp + fn)
    val f1 = if ((prec + rec) == 0) 0.0 else 2 * (prec * rec) / (prec + rec)
    System.err.println(prefix + "\t" + iteration + "\t" + tp.toInt + "\t" + fp.toInt + "\t" + fn.toInt + "\t" + df.format(prec) + "\t" + df.format(rec) + "\t" + df.format(f1))
    f1
  }

  def DEBUGevaluateCorpusOnEvents(corpus: Array[Document]): Unit = {
    var tp, fp, fn = 0.0
    for (doc <- corpus; (sentence: Sentence) <- doc) {
      val A = sentence.eval(false, false, false, null, false, false, false)
      val B = sentence.eval(true, false, false, null, false, false, false)
      val C = sentence.eval(true, false, true, null, false, false, false)
      if (B._6 > A._6 || C._6 > B._6 || C._6 > A._6) {
        System.err.println("----- Problematic Evaluation -----")
        System.err.println(A + " < " + B + " < " + C)
        System.err.println(sentence.toString(true))
        System.err.println("Events:")
        for (event <- sentence.events) {
          System.err.println("    " + event + " with arguments " + event.arguments)
        }
        System.err.println("Genes:")
        for (gene <- sentence.genes) {
          System.err.println("    " + gene)
        }
        System.err.println("True Events:")
        for (event <- sentence.trueSentence.events) {
          System.err.println("    " + event + " with arguments " + event.arguments)
        }
        System.err.println("----------------------------------")
      }
    }
  }

  //  def evaluateCorpusOnEventsOld(prefix:String, iteration: Int, corpus: Array[Document],evalEventType:Boolean,evalEventArgument:Boolean) : Double = {
  //    var tp, fp, fn = 0.0
  //    for(doc <- corpus ; sentence <- doc) {
  //      for(event <- sentence.events) {
  //        if (event.asInstanceOf[Event].isATrueEventInSentence(evalEventType,evalEventArgument,null))
  //          tp += 1.0 ;
  //        else
  //          fp += 1.0 ;
  //      }
  //      fn += sentence.trueSentence.numberOfMissedEvents(evalEventType,evalEventArgument,null)
  //    }
  //    val prec = if ((tp+fp)==0) 1.0 else tp/(tp+fp)
  //    val rec = if ((tp+fn)==0) 0.0 else tp/(tp+fn)
  //    val f1 = if ((prec+rec)==0) 0.0 else 2*(prec*rec)/(prec+rec)
  //    System.err.println(prefix+"\t"+iteration+"\t"+tp.toInt+"\t"+fp.toInt+"\t"+fn.toInt+"\t"+df.format(prec)+"\t"+df.format(rec)+"\t"+df.format(f1))
  //    f1
  //  }

  // macro averaging!
  def evaluateCorpusOnClues(prefix: String, iteration: Int, corpus: Array[Document]): Double = {
    var tp, fp, fn = 0.0
    for (doc <- corpus; (sentence: Sentence) <- doc) {
      val (atp, afp, afn, aprec, arec, af1) = sentence.evalClue(null, false, 1, false)
      tp += atp
      fp += afp
      fn += afn
    }
    val prec = if ((tp + fp) == 0) 1.0 else tp / (tp + fp)
    val rec = if ((tp + fn) == 0) 0.0 else tp / (tp + fn)
    val f1 = if ((prec + rec) == 0) 0.0 else 2 * (prec * rec) / (prec + rec)
    System.err.println(prefix + "\t" + iteration + "\t" + tp.toInt + "\t" + fp.toInt + "\t" + fn.toInt + "\t" + df.format(prec) + "\t" + df.format(rec) + "\t" + df.format(f1))
    f1
  }

  def readOneExampleFile() {
    val doc = readOneFile("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/sample/9893043")
    StanfordDependencies.attachToDoc("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/sample/sd/9893043.txt", doc)
    POSReader.attachToDoc("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/sample/ptree/9893043.txt", doc)
    writeDocumentToFiles(doc,
      "/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tmp/9893043.txt",
      "/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tmp/9893043.a1",
      "/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tmp/9893043.a2", false)
    // some debugging output
    println(doc(0).foreach(println))
  }

  /**
   * Train and Test Workflow. In developement
   */
  def testEventPutInAndOut {
    // Read training and testing data.
    val trainDocuments = readCorpus("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tiny");
    // val testDocuments  = readCorpus("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/development") ;


    Domain[EventFeatureVector].maxSize = 500000
    // the model
    val model = new Model
    //val eventTemplate = new EventTemplate
    //eventTemplate.init
    model += (new EventTemplate).init


    // the objective (also as a model)
    val objective = new Model
    val sentenceObj = new TemplateWithStatistics1[Sentence] {
      def score(s: Stat) = {
        //System.err.println(">>"+ s)
        val currentSentence = s.s1.asInstanceOf[Sentence]
        // how many events do we score in this sentence, are they weirdly changing?
        val r = currentSentence.eval(false, false, false, false)
        Workflow.deberr("Objective: Events (" + currentSentence.events.length + ")-->" + currentSentence.events + " returning " + r)
        r._6
      }
    }
    objective += sentenceObj

    val sentence = trainDocuments(0)(0)
    /*    var da = new DiffList
    val clue1 = new Span(sentence, 0, 2, null, "T9999")(da)
    val eventType1 = new EventType()
    eventType1.set("Bla")(da)
    val eventTmp1 = new Event("E9999",clue1,eventType1)(da)
    clue1.addEntity(eventTmp1)
    System.err.println(sentence.spans)
    System.err.println(sentence.events)
    System.err.println("--------")
    var db = new DiffList
    val clue2 = new Span(sentence, 0, 2, null, "T9999")(db)
    val eventType2 = new EventType()
    eventType2.set("Bla")(db)
    val eventTmp2 = new Event("E9999",clue2,eventType2)(db)
    clue2.addEntity(eventTmp2)
    System.err.println(sentence.spans)
    System.err.println(sentence.events)
    System.err.println("--------")*/

    //System.exit(1)
    var d1 = new DiffList
    // add some true event
    System.err.println("True Event adding:")
    sentence.addNewRandomEvent(7)(d1)

    System.err.println("Events:              " + sentence.events)
    System.err.println("DiffList:            " + d1)
    sentence.eval(true, false, false, System.err, false, false, false)
    //    val r1 = d1.scoreAndUndo(model,objective)
    //    System.err.println("Score and Undo: "+r1)
    //    System.err.println("Events after Undo:   "+sentence.events)
    //    System.err.println("DiffList after Undo: "+d1)
    //    sentence.eval(System.err)

    System.err.println("\n")
    val d2 = new DiffList
    // add a wrong event
    System.err.println("Wrong Event adding")
    sentence.addNewRandomEvent(0)(d2)

    System.err.println("Events: " + sentence.events)
    System.err.println("DiffList: " + d2)
    sentence.eval(true, false, false, System.err, false, false, false)
    val r2 = d2.scoreAndUndo(model, objective)
    System.err.println("Score and Undo: " + r2)
    System.err.println("Events after Undo:   " + sentence.events)
    System.err.println("DiffList after Undo: " + d2)
    sentence.eval(true, false, false, System.err, false, false, false)
    System.err.println("Redo!")
    d2.redo
    System.err.println("Events after Redo:   " + sentence.events)
    System.err.println("Spans: " + sentence.spans)
    System.err.println("DiffList after Redo: " + d2)
    sentence.eval(true, false, false, System.err, false, false, false)
    // manually remove the correct event

    System.err.println("\n")
    System.err.println("Remove an Event manually")
    val d3 = new DiffList
    // now we try the other proposal, removing an event. We remove first a wrong event
    val event = sentence.events(0)
    System.err.println(">>>>> EVENT to REMOVE : " + event)
    sentence.removeEvent(event, true)(d3)
    event.delete(d3)
    System.err.println("Events: " + sentence.events)
    System.err.println("Spans: " + sentence.spans)
    System.err.println("DiffList: " + d3)
    sentence.eval(true, false, false, System.err, false, false, false)
    val r3 = d3.scoreAndUndo(model, objective)
    System.err.println("Score and Undo: " + r3)
    System.err.println("Events after Undo:   " + sentence.events)
    System.err.println("Spans: " + sentence.spans)
    System.err.println("DiffList after Undo: " + d3)
    sentence.eval(true, false, false, System.err, false, false, false)
    System.err.println("Redo!")
    d3.redo
    System.err.println("Events after Redo:   " + sentence.events)
    System.err.println("Spans: " + sentence.spans)
    System.err.println("DiffList after Redo: " + d3)
    sentence.eval(true, false, false, System.err, false, false, false)
  }

  def testSpanPutInAndOut {
    // Read training and testing data.
    val trainDocuments = readCorpus("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tiny");
    // val testDocuments  = readCorpus("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/development") ;

    // the model
    val model = new Model

    // the objective (also as a model)
    val objective = new Model
    objective += new TemplateWithStatistics1[Sentence] {def score(s: Stat) = {s.s1.asInstanceOf[Sentence].evalClue(null, true, 1, false)._6}}

    val sentence = trainDocuments(0)(0)

    var d1 = new DiffList
    // add some true event
    System.err.println("True Span adding:")
    val s1 = new Span(sentence, 7, 1, null, "ID1")(d1)
    d1 = new DiffList
    val e1 = sentence.addNewEvent(-1, -1, "Negative_regulation", s1)(d1)
    System.err.println("Present1: " + e1.present)
    System.err.println("Spans: " + sentence.spans)
    System.err.println("Events: " + sentence.events)
    System.err.println("DiffList:            " + d1)
    sentence.evalClue(System.err, false, 1, false)
    sentence.eval(true, false, false, System.err, false, false, false)

    val r1 = d1.scoreAndUndo(model, objective)
    System.err.println("Score and Undo: " + r1)
    System.err.println("Present1: " + e1.present)
    System.err.println("Spans after Undo:   " + sentence.spans)
    System.err.println("Evants after Undo:   " + sentence.events)
    System.err.println("DiffList after Undo: " + d1)
    sentence.evalClue(System.err, false, 1, false)
    sentence.eval(true, false, false, System.err, false, false, false)

    System.err.println("Redo!")
    d1.redo
    System.err.println("Present1: " + e1.present)
    System.err.println("Spans after Redo:   " + sentence.spans)
    System.err.println("Spans after Redo:   " + sentence.events)
    System.err.println("DiffList after Redo: " + d1)
    sentence.evalClue(System.err, false, 1, false)
    sentence.eval(true, false, false, System.err, false, false, false)

    val d1a = new DiffList
    System.err.println("Remove Event")
    sentence.removeEvent(e1, false)(d1a)
    System.err.println("Present1: " + e1.present)
    System.err.println("Spans: " + sentence.spans)
    System.err.println("Events: " + sentence.events)
    sentence.evalClue(System.err, false, 1, false)
    sentence.eval(true, false, false, System.err, false, false, false)

    val r1a = d1a.scoreAndUndo(model, objective)
    System.err.println("Score and Undo: " + r1a)
    System.err.println("Present1: " + e1.present)
    System.err.println("Spans after Undo:   " + sentence.spans)
    System.err.println("Events after Undo:   " + sentence.events)
    System.err.println("DiffList after Undo: " + d1a)
    sentence.evalClue(System.err, false, 1, false)
    sentence.eval(true, false, false, System.err, false, false, false)

    System.err.println("Redo!")
    d1a.redo
    System.err.println("Present1: " + e1.present)
    System.err.println("Spans after Redo:   " + sentence.spans)
    System.err.println("Events after Redo:   " + sentence.events)
    System.err.println("DiffList after Redo: " + d1a)
    sentence.evalClue(System.err, false, 1, false)
    sentence.eval(true, false, false, System.err, false, false, false)


    System.exit(1)
    System.err.println("\n")
    val d2 = new DiffList
    // add a wrong event
    System.err.println("Wrong Span adding")
    new Span(sentence, 0, 1, null, "ID2")(d2)
    System.err.println("Spans: " + sentence.spans)
    System.err.println("DiffList: " + d2)
    sentence.evalClue(System.err, false, 1, false)

    val r2 = d2.scoreAndUndo(model, objective)
    System.err.println("Score and Undo: " + r2)
    System.err.println("Spans after Undo:   " + sentence.spans)
    System.err.println("DiffList after Undo: " + d2)
    sentence.evalClue(System.err, false, 1, false)
    System.err.println("Redo!")
    d2.redo
    System.err.println("Spans after Redo:   " + sentence.spans)
    System.err.println("DiffList after Redo: " + d2)
    sentence.evalClue(System.err, false, 1, false)
    // manually remove the correct event

    System.err.println("\n")
    System.err.println("Remove a Span manually")
    val d3 = new DiffList
    // now we try the other proposal, removing an event. We remove first a wrong event
    val span = sentence.spans.filter(s => s.isOfEvent)(0)
    System.err.println(">>>>> EVENT to REMOVE : " + span)
    sentence.removeSpan(span)(d3)
    System.err.println("Spans: " + sentence.spans)
    System.err.println("DiffList: " + d3)
    sentence.evalClue(System.err, false, 1, false)
    val r3 = d3.scoreAndUndo(model, objective)
    System.err.println("Score and Undo: " + r3)
    System.err.println("Spans after Undo:   " + sentence.spans)
    System.err.println("DiffList after Undo: " + d3)
    sentence.evalClue(System.err, false, 1, false)
    System.err.println("Redo!")
    d3.redo
    System.err.println("Spans after Redo:   " + sentence.spans)
    System.err.println("DiffList after Redo: " + d3)
    sentence.evalClue(System.err, false, 1, false)
  }

  def readAndWrite {
    val trainDocuments = readCorpus("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tiny");
    val pre = "/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tmp/"
    writeDocumentToFiles(trainDocuments(0), pre + "1.txt", pre + "1.a1", pre + "1.a2", true)
  }

  def sampleSubsetFromCorpus(documents: Array[Document], numToKeep: Int): Array[Document] = {
    System.err.println("Sub-sampling, number of instances to keep: " + numToKeep)
    if (numToKeep > documents.size)
      System.err.println("Warning: Demanded number of documents exceeds corpus size, " +
              "continuing with " + documents.size + " documents!")
    val instancelist = java.util.Arrays.asList(documents.toArray: _*)
    java.util.Collections.shuffle(instancelist, new java.util.Random(1))
    var i = 0
    var subset = new ArrayBuffer[Document]
    for (i <- 0 until Math.min(numToKeep, documents.size))
      subset += instancelist.get(i)
    return subset.toArray
  }

  def cvSets(instances: Array[Document], numFolds: Int): Array[Array[Document]] = {
    System.err.println("CV Set-Sampling with " + numFolds + " folds from " + instances.size + " instances:")
    // how many do we want to have in each fold?
    val numInEachFold = instances.size / numFolds
    System.err.println("Number of instances in each fold: " + numInEachFold)
    // shuffle instances
    val instancelist = java.util.Arrays.asList(instances.toArray: _*)
    java.util.Collections.shuffle(instancelist, new java.util.Random(1))
    val folds = new Array[Array[Document]](numFolds)
    var i = 0
    var currentFold = new ArrayBuffer[Document]
    var currentFoldIndex = 0
    while (i < instances.size) {
      currentFold += instancelist.get(i)
      i += 1
      if (i % numInEachFold == 0) {
        folds(currentFoldIndex) = currentFold.toArray
        currentFold = new ArrayBuffer[Document]
        currentFoldIndex += 1
      }
    }
    folds
  }
  // return documents for training (_1) and for validation (_2)
  def combineSubSampleToTrainingAndValidation(folds: Array[Array[Document]], asValidation: Int): (Array[Document], Array[Document]) = {
    var trainDocs = new ArrayBuffer[Document]
    var valDocs = new ArrayBuffer[Document]
    for (index <- 0 until folds.size) {
      if (index == asValidation) valDocs ++= folds(index) else trainDocs ++= folds(index)
    }
    (trainDocs.toArray, valDocs.toArray)
  }

  // TODO add event theme evaluation, not all arguments
  def showAllFPEvents(corpus: Array[Document], clue: Boolean, cluetype: Boolean, cluetypetheme: Boolean, cluetypearg: Boolean) {
    System.err.println("FP:")
    if (clue) {
      System.err.println("FP: Event Clue is wrong:")
      val notToCount = new HashSet[Event]
      for (doc <- corpus; (sentence: Sentence) <- doc; (event: Event) <- sentence.events) {
        if (!event.isATrueEventInSentence(false, false, false, notToCount)) {
          System.err.println(event)
        }
      }
    }
    if (cluetype) {
      System.err.println("\nFP: Event Clue or Type is wrong:")
      val notToCount = new HashSet[Event]
      for (doc <- corpus; (sentence: Sentence) <- doc; (event: Event) <- sentence.events) {
        if (!event.isATrueEventInSentence(true, false, false, notToCount)) {
          System.err.println(event)
        }
      }
    }
    if (cluetypetheme) {
      System.err.println("\nFP: Event Clue or Type or Theme are wrong:")
      val notToCount = new HashSet[Event]
      for (doc <- corpus; (sentence: Sentence) <- doc; (event: Event) <- sentence.events) {
        if (!event.isATrueEventInSentence(true, true, false, notToCount)) {
          System.err.println(event + " with Argument " + event.arguments.map(_.entity.span.spannedTokens.map(_.word).mkString(" ")))
        }
      }
    }
    if (cluetypearg) {
      System.err.println("\nFP: Event Clue or Type or Arguments are wrong:")
      val notToCount = new HashSet[Event]
      for (doc <- corpus; (sentence: Sentence) <- doc; (event: Event) <- sentence.events) {
        if (!event.isATrueEventInSentence(true, false, true, notToCount)) {
          System.err.println(event + " with Argument " + event.arguments.map(_.entity.span.spannedTokens.map(_.word).mkString(" ")))
        }
      }
    }
  }

  def showAllResultsPerSentence(corpus: Array[Document], checkType: Boolean, checkTheme: Boolean, checkArguments: Boolean, tpfnfp: Boolean, onlyproblematic: Boolean) {
    for (doc <- corpus; (sentence: Sentence) <- doc) {
      val r = sentence.eval(checkType, checkTheme, checkArguments, null, false, false, false) // for deciding if it should be printed
      if (!tpfnfp) {
        //        System.err.println("===="+(sentence.trueSentence.events.length +",.,.,."+ f))
        if (!onlyproblematic || (sentence.trueSentence.events.length > 0 && r._6 != 1.0)) {
          System.err.println()
          sentence.eval(checkType, checkTheme, checkArguments, System.err, false, false, false) // for printing
          System.err.println("Sentence: " + sentence.toString(true))
          System.err.println("True:")
          for ((event: Event) <- sentence.trueSentence.events) {
            System.err.print(event.outputFormatE.trim + " with clue " + event.outputFormatT)
            event.arguments.foreach(a => System.err.print("\t" + a.entity.genericOutputFormatTorE))
          }
          System.err.println("Predicted:")
          for ((event: Event) <- sentence.events) {
            System.err.print(event.isATrueEventInSentence(checkType, checkTheme, checkArguments, null) + ":" + event.outputFormatE.trim + " with clue " + event.outputFormatT)
            event.arguments.foreach(a => System.err.print("\t" + a.entity.genericOutputFormatTorE))
          }
          //System.err.println("All Spans:")
          //System.err.println(sentence.spans)
        }
      } else {
        val f = sentence.eval(checkType, checkTheme, checkArguments, System.err, false, false, false)
        System.err.println("\nSentence: " + sentence.map(_.word).mkString(" "))
        System.err.println("TP")
        for ((event: Event) <- sentence.trueSentence.events;
             if (sentence.events.find(varEvent => varEvent.deepEqual(checkType, checkTheme, checkArguments, event)).isDefined)) {
          System.err.println(event + " with Argument " + event.arguments.map(_.entity.span.spannedTokens.map(_.word).mkString(" ")))
        }
        System.err.println("FN")
        for ((event: Event) <- sentence.trueSentence.events;
             if (!sentence.events.find(varEvent => varEvent.deepEqual(checkType, checkTheme, checkArguments, event)).isDefined)) {
          System.err.println(event + " with Argument " + event.arguments.map(_.entity.span.spannedTokens.map(_.word).mkString(" ")))
        }
        System.err.println("FP")
        val notToCount = new HashSet[Event]
        for ((event: Event) <- sentence.events) {
          if (!event.isATrueEventInSentence(checkType, checkTheme, checkArguments, notToCount)) {
            System.err.println(event + " with Argument " + event.arguments.map(_.entity.span.spannedTokens.map(_.word).mkString(" ")))
          }
        }
      }
    }
  }

  def writeCorpus(corpus: Array[Document], folder: String, writeTrueSentences: Boolean) {
    System.err.println("Writing " + corpus.length + " documents to " + folder)
    new File(folder).mkdirs
    corpus.foreach((d: Document) => Data.writeDocumentToFiles(d, folder + "/" + d.id + ".txt", folder + "/" + d.id + ".a1", folder + "/" + d.id + ".a2.t1", writeTrueSentences))
  }

  def showAllCluesPerSentence(corpus: Array[Document]) {
    for (doc <- corpus; (sentence: Sentence) <- doc) {
      System.err.println("\nSentence: " + sentence.map(_.word).mkString(" "))
      sentence.evalClue(System.err, false, 1, false)
      System.err.println("True:")
      for ((span: Span) <- sentence.trueSentence.spans.filter(s => s.isOfEvent))
        System.err.println(span.map(_.word).mkString(" "))
      System.err.println("Predicted:")
      for ((span: Span) <- sentence.spans.filter(s => s.isOfEvent))
        System.err.println(span.map(_.word).mkString(" "))
    }
  }

  // TODO add theme eval
  def showAllFNEvents(corpus: Array[Document], step1: Boolean, step2: Boolean, step3: Boolean) {
    System.err.println("FN:")
    if (step1) {
      System.err.println("FN: Event Clue is wrong:")
      for (doc <- corpus;
           (sentence: Sentence) <- doc;
           (event: Event) <- sentence.trueSentence.events;
           if (!sentence.events.find(varEvent => varEvent.deepEqual(false, false, false, event)).isDefined)) {
        System.err.println(event)
      }
    }
    if (step2) {
      System.err.println("\nFN: Event Clue or Type is wrong:")
      for (doc <- corpus;
           (sentence: Sentence) <- doc;
           (event: Event) <- sentence.trueSentence.events;
           if (!sentence.events.find(varEvent => varEvent.deepEqual(true, false, false, event)).isDefined)) {
        System.err.println(event)
      }
    }
    if (step3) {
      System.err.println("\nFN: Event Clue or Type or Arguments are wrong:")
      for (doc <- corpus;
           (sentence: Sentence) <- doc;
           (event: Event) <- sentence.trueSentence.events;
           if (!sentence.events.find(varEvent => varEvent.deepEqual(true, false, true, event)).isDefined)) {
        System.err.println(event + " with Argument " + event.arguments.map(_.entity.span.spannedTokens.map(_.word).mkString(" ")))
      }
    }
  }

  def testArgumentAdding {
    val trainDocuments = readCorpus("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tiny");
    Domain[EventFeatureVector].maxSize = 500000
    val model = new Model
    model += (new EventTemplate).init

    val sentence = trainDocuments(0)(0)
    System.err.println("All true events and genes")
    (sentence.trueSentence.events ++ sentence.trueSentence.genes).foreach(e => System.err.println(e + "   " + (if (e.isInstanceOf[Event]) e.asInstanceOf[Event].arguments else "")))
    System.err.println()

    System.err.print("True Event adding to sentence ")
    var d1 = new DiffList
    val trueEvent1 = sentence.addNewEvent(24, 1, "Negative_regulation", null)(d1)
    System.err.println(trueEvent1)
    System.err.print("True Event adding to sentence ")
    val trueEvent2 = sentence.addNewEvent(31, 1, "Binding", null)(d1)
    System.err.println(trueEvent2)

    System.err.println("Events in sentence: " + sentence.events)
    System.err.println("DiffList: " + d1)
    System.err.print("Eval1 w/o Arg: ");
    sentence.eval(true, false, false, System.err, false, false, false)
    System.err.print("Eval2 w/  Arg: ");
    sentence.eval(true, false, true, System.err, false, false, false)
    System.err.println()

    System.err.println("The second is a theme of the first! Let's try to model that.")
    var d2 = new DiffList
    System.err.println("Before:")
    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d2)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println()
    System.err.println("Now adding an event as correct argument")
    val arg = trueEvent1.addEntityAsArgument(trueEvent2, "Theme")(d2)
    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d2)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println()
    System.err.println("UNDO")
    d2.undo
    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d2)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println()
    System.err.println("REDO")
    d2.redo
    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d2)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println("Evaluation:")
    System.err.print("Eval1 w/o Arg: ");
    sentence.eval(true, false, false, System.err, false, false, false)
    System.err.print("Eval2 w/  Arg: ");
    sentence.eval(true, false, true, System.err, false, false, false)

    System.err.println()
    System.err.println()
    val d3 = new DiffList
    System.err.println("Now adding a gene as correct argument")
    val gene = trueEvent1.clue.sentence.genes.find(_.span.start == 4).get
    val arg1 = trueEvent1.addEntityAsArgument(gene, "Cause")(d3)

    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d3)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println()
    System.err.println("UNDO")
    d3.undo
    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d3)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println()
    System.err.println("REDO")
    d3.redo
    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d3)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println("Evaluation:")
    System.err.print("Eval1 w/o Arg: ");
    sentence.eval(true, false, false, System.err, false, false, false)
    System.err.print("Eval2 w/  Arg: ");
    sentence.eval(true, false, true, System.err, false, false, false)

    System.err.println()
    System.err.println()
    val d4 = new DiffList
    System.err.println("Now let's remove the firstly added event as an argument (" + trueEvent2 + ")")
    // find the argument, it is not the same pointer any more. WHY? Because of undo and redo?
    val argToRemove = trueEvent1.arguments.find((a: Argument) => a.entity.deepEqual(trueEvent2)).get
    trueEvent1.removeEntityAsArgument(argToRemove)(d4)

    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d4)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println()
    System.err.println("UNDO")
    d4.undo
    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d4)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println()
    System.err.println("REDO")
    d4.redo
    System.err.println("All arguments in event: " + trueEvent1.arguments + " (present? " + trueEvent1.arguments.map(_.present).mkString(",") + ")")
    System.err.println("DiffList: " + d4)
    trueEvent1.evalFMeasureOfArguments(System.err)

    System.err.println("Evaluation:")
    System.err.print("Eval1 w/o Arg: ");
    sentence.eval(true, false, false, System.err, false, false, false)
    System.err.print("Eval2 w/  Arg: ");
    sentence.eval(true, false, true, System.err, false, false, false)
  }

  def smalltestSpanPutInAndOut {
    val docs = Data.readCorpus("data/corpora/tiny")
    val s: Sentence = docs(0)(0)
    println(s.toString(true))
    println("True Genes " + s.trueSentence.genes)
    println("True Events " + s.trueSentence.events)
  }

  def evaluationTesting() {
    val docs = Data.readCorpus("data/corpora/tiny4")
    val s: Sentence = docs(0)(11)
    // make weird finding manually
    val d = new DiffList
    val event1 = s.addNewEvent(0, 3, "Positive_regulation", null)(d)
    event1.addEntityAsArgument(s.genes(0), "Theme")(d)
    val event2 = s.addNewEvent(0, 3, "Gene_expression", null)(d)
    event2.addEntityAsArgument(s.genes(0), "Theme")(d)

    System.err.println("----- Problematic Evaluation -----")
    s.printStructure()
    System.err.println("----------------------------------")

    System.err.println("false,false")
    s.eval(false, false, false, System.err, false, true, false)
    System.err.println("true,false")
    s.eval(true, false, false, System.err, false, true, false)
    System.err.println("true,true")
    s.eval(true, false, true, System.err, false, true, false)
  }

  def evaluationTestingForBinding() {
    val docs = Data.readCorpus("data/corpora/small")
    val s: Sentence = docs(0)(1)
    // make weird finding manually
    println(s.map(_.word).mkString(" "))

    val d = new DiffList
    val event1 = s.addNewEvent(5,1, "Binding", null)(d)
    event1.addEntityAsArgument(s.genes(2), "Theme")(d)
//    event1.addEntityAsArgument(s.genes(1), "Theme")(d)

    val event2 = s.addNewEvent(5,1, "Binding", null)(d)
//    event2.addEntityAsArgument(s.genes(2), "Theme")(d)
    event2.addEntityAsArgument(s.genes(1), "Theme")(d)

//    val event2 = s.addNewEvent(0, 3, "Gene_expression", null)(d)
//    event2.addEntityAsArgument(s.genes(0), "Theme")(d)

    System.err.println("----- Problematic Evaluation -----")
    s.printStructure()
    System.err.println("----------------------------------")

    System.err.println("false,false")
    s.eval(false, false, false, System.err, false, true, false)
    System.err.println("true,false")
    s.eval(true, false, false, System.err, false, true, false)
    System.err.println("true,true")
    s.eval(true, false, true, System.err, false, true, false)
  }


  def readAndWriteTest {
    // read the problematic file
    val prefix = "/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/development/9164948"
    val doc = Data.readOneFile(prefix)

    val writep = "/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/predict/test/9164948"
    Data.writeDocumentToFiles(doc, writep + ".txt", writep + ".a1", writep + ".a2.t1", true)
  }

  def themeTest() {
    InitFile.initialize("/home/rklinger/work/mercurial/refectorie/proj/bionlp/init/dev2.ini")
    val doc = Data.readCorpus("/home/rklinger/work/mercurial/refectorie/proj/bionlp/data/corpora/tiny")
    val s = doc(0)(0)
    System.err.println(s.toString(true))
    val clue = new Span(s, 2, 1, null, "000")(null)
    System.err.println(clue)
    val e = s.addNewEvent(-1, -1, "Positive_regulation", clue)(null)
    System.err.println(e.outputFormatE + "\t" + e.outputFormatT)
    val t = s.genes.find((g: Gene) => g.span.stringrepr == "STAT6").get
    System.err.println(t.outputFormat)
    val a = e.addEntityAsArgument(t, "Theme")(null)
    val b = a.isATrueArgumentInEvent(e, true)
    System.err.println("Good argument: " + b)
    val c = e.isATrueEventInSentence(false, false, false, null)
    val spantrue = e.isATrueEventInSentence(true, false, false, null)
    val spanthemetrue = e.isATrueEventInSentence(true, true, false, null)
    val spanargNOTtheme = e.isATrueEventInSentence(true, false, true, null)
    val all = e.isATrueEventInSentence(true, true, true, null)
    System.err.println(spantrue + " " + spanthemetrue + " " + spanargNOTtheme + " " + all)
  }

  def main(args: Array[String]): Unit = {
    //run
    //readAndWrite
    //testArgumentAdding
    //testSpanPutInAndOut
    //smalltestSpanPutInAndOut
    //evaluationTesting
    evaluationTestingForBinding
    //readAndWriteTest
    //testArgumentAdding
    //themeTest
//    val c = readCorpus("data/corpora/sample")
//    InitFile.initialize("init/dev.ini")
//    for (d: Document <- c) System.err.println(d.id + ":" + d.mostImportantGene)
  }
}
