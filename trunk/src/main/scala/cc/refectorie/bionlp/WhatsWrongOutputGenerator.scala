package cc.refectorie.bionlp

import java.io.PrintStream
import cc.refectorie.bionlp.SyntacticPatternTemplate.Pattern
import collection.mutable.HashSet

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Sebastian Riedel
 * License: GPL2
 */

object WhatsWrongOutputGenerator {
  def quote(text: String) = "\"" + text + "\""

  def write(docs: Seq[Document], out: PrintStream) {
    for (doc <- docs; sentence <- doc) write(sentence, out)
  }

  def writeGold(docs: Seq[Document], out: PrintStream) {
    for (doc <- docs; sentence <- doc) write(sentence.trueSentence, out)
  }

  def write(sentence: Sentence, out: PrintStream) {

    out.println(">>")

    out.println(">word")
    for (token <- sentence) {
      out.println("%d\t%s".format(token.position, quote(token.word)))
    }
    out.println(">pos")
    for (token <- sentence) {
      out.println("%d\t%s".format(token.position, quote(token.pos)))
    }
    out.println(">protein")
    for (gene <- sentence.genes) {
      out.println("%d\t%d\t%s".format(gene.span.first.position, gene.span.last.position, quote("Protein")))
    }

    out.println(">clue")
    val addedClues = new HashSet[(Int, Int, String)]

    for (event <- sentence.events) {
      if (!addedClues((event.span.first.position, event.span.last.position, event.eventType.value))) {
        val patterns = SyntacticPatternTemplate.extractFilteredCluePatterns(event).
                map(_.toStringWithStats).mkString("-BR-", "-BR-", "")
        out.println("%d\t%d\t%s\t%s".format(event.span.first.position, event.span.last.position,
          quote(event.eventType.value), quote(patterns)))
        addedClues += ((event.span.first.position, event.span.last.position, event.eventType.value))
      }
    }

    out.println(">role")
    val addedRoles = new HashSet[(Token, Token, String)]
    for (event <- sentence.events) {
      for (arg <- event.arguments;
           if (!addedRoles((event.span.mainClueToken, arg.entity.span.head, arg.roleType.value)))) {
        val patterns = SyntacticPatternTemplate.extractFilteredPatterns(event, arg).
                map(_.toStringWithStats).mkString("-BR-", "-BR-", "")
        out.println("%d\t%d\t%s\t%s".format(
          event.span.mainClueToken.position, arg.entity.span.head.position, quote(arg.roleType.value), quote(patterns)))
        addedRoles += ((event.span.mainClueToken, arg.entity.span.head, arg.roleType.value))
      }
    }
    out.println(">sameEvent")
    for (event <- sentence.events) {
      for (arg1 <- event.arguments; arg2 <- event.arguments;
           if (arg1.entity.span.head.position < arg2.entity.span.head.position)) {
        val patterns = SyntacticPatternTemplate.extractFilteredPatterns(event, arg1,arg2).
                map(_.toStringWithStats).mkString("-BR-", "-BR-", "")
        out.println("%d\t%d\t%s\t%s".format(
          arg1.entity.span.head.position, arg2.entity.span.head.position,
          quote("%s/%s".format(arg1.roleType.value,arg2.roleType.value)), 
          quote(patterns)))
      }
    }
    out.println(">dep")
    for (head <- sentence.dependencyPaths.keySet;
         modifier <- sentence.dependencyPaths(head).keySet) {
      val path = sentence.dependencyPaths(head)(modifier)
      if (path != null && path.size == 1 && path(0)._2) {
        out.println("%d\t%d\t%s".format(head.position, modifier.position, quote(path(0)._1.dependencyType)))
      }
    }

    out.println(">info")
    out.println("%d\t%d\t%s".format(0, sentence.size - 1, quote("Doc: " + sentence.doc.id)))
    out.println("%d\t%d\t%s".format(0, sentence.size - 1, quote("Sen: " + sentence.position)))

    out.println(">index")
    for (token <- sentence) {
      out.println("%d\t%s".format(token.position, quote(token.position.toString)))
    }

    out.println("")
  }


}