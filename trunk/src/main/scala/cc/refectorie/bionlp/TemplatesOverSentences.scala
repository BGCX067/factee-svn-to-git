package cc.refectorie.bionlp

import collection.mutable.{HashMap}
import cc.factorie.{DotStatistics1, Template2, BinaryVectorVariable}
import cc.refectorie.bionlp.Templates.AllowWeightStats
import cc.refectorie.bionlp.TemplatesForPieceWise.EventEventSharingArgFeatureVector

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object TemplatesOverSentences {

  class EventsInDifferentSentencesFeatureVector(e1:Event,e2:Event) extends BinaryVectorVariable[String] {
    if (e1.clue.sentence.position == e2.clue.sentence.position) throw new Exception("Events are not from different sentences!")
    // the events are from different sentences, for sure! Do they have something in common?
    if (InitFile.sameClueString &&
            e1.clue.stringrepr == e2.clue.stringrepr &&
            e1.eventType.value == e2.eventType.value) {
      this += "Events Have Same string representation"
      this += "Events Have Same string representation "+e1.eventType.value
      this += "Events Have Same string representation "+e1.eventType.value+":"+e1.clue.stringrepr
    }

    if (InitFile.sameClueSameTheme &&
            e1.clue.stringrepr == e2.clue.stringrepr &&
            e1.eventType.value == e2.eventType.value &&
            e1.getThemes.length > 0 &&
            e1.getThemes.length == e2.getThemes.length &&
            e1.getThemes.forall(a1 => e2.getThemes.exists(a2 => a1.entity.span.stringrepr == a2.entity.span.stringrepr))
      ) {
      this += "Events Have Same string representation and same theme string"
      this += "Events Have Same string representation and same theme string "+e1.eventType.value
      this += "Events Have Same string representation and same theme string "+e1.eventType.value+":"+e1.clue.stringrepr
      this += "Events Have Same string representation and same theme string "+e1.eventType.value+":"+e1.clue.stringrepr+
              ":"+e1.getThemes.map((t:Argument) => (if (t.entity.isInstanceOf[Gene]) "Gene" else t.entity.asInstanceOf[Event].eventType.value)).mkString(":")
      this += "Events Have Same string representation and same theme string "+e1.eventType.value+":"+e1.clue.stringrepr+
              ":"+e1.getThemes.map((t:Argument) => (if (t.entity.isInstanceOf[Gene]) "Gene" else t.entity.asInstanceOf[Event].eventType.value)).mkString(":")+
              ":"+e1.getThemes.map((t:Argument) => (if (t.entity.isInstanceOf[Gene]) "Gene" else t.entity.span.stringrepr)).mkString(":")
    }

    if (InitFile.sameClueSameArgs &&
            e1.clue.stringrepr == e2.clue.stringrepr &&
            e1.eventType.value == e2.eventType.value &&
            e1.arguments.length > 0 &&
            e1.arguments.length == e2.getThemes.length &&
            e1.arguments.forall(a1 => e2.getThemes.exists(a2 => a1.entity.span.stringrepr == a2.entity.span.stringrepr))
      ) {
      this += "Events Have Same string representation and same args string"
      this += "Events Have Same string representation and same args string "+e1.eventType.value
      this += "Events Have Same string representation and same args string "+e1.eventType.value+":"+e1.clue.stringrepr
      this += "Events Have Same string representation and same args string "+e1.eventType.value+":"+e1.clue.stringrepr+
              ":"+e1.arguments.map((t:Argument) => t.roleType.value+":"+(if (t.entity.isInstanceOf[Gene]) "Gene" else t.entity.asInstanceOf[Event].eventType.value)).mkString(":")
      this += "Events Have Same string representation and same args string "+e1.eventType.value+":"+e1.clue.stringrepr+
              ":"+e1.arguments.map((t:Argument) => t.roleType.value+":"+(if (t.entity.isInstanceOf[Gene]) "Gene" else t.entity.asInstanceOf[Event].eventType.value)).mkString(":")+
              ":"+e1.arguments.map((t:Argument) => t.roleType.value+":"+(if (t.entity.isInstanceOf[Gene]) "Gene" else t.entity.span.stringrepr)).mkString(":")
    }
    //debug
//    if (    e1.clue.stringrepr == e2.clue.stringrepr &&
//            e1.eventType.value == e2.eventType.value) {
//      System.err.println(e1.outputFormatE.trim+"\t"+e1.outputFormatT.trim+"\t"+e1.arguments.map(a => a.roleType.value+":"+a.entity.genericOutputFormatT.trim).mkString(" "))
//      System.err.println(e2.outputFormatE.trim+"\t"+e2.outputFormatT.trim+"\t"+e2.arguments.map(a => a.roleType.value+":"+a.entity.genericOutputFormatT.trim).mkString(" "))
//      for(v <- this.values) System.err.println(v)
//      System.err.println("-------")
//    }
  }

  class EventsInDifferentSentencesTemplate extends Template2[Event,Event] with DotStatistics1[EventsInDifferentSentencesFeatureVector] with AllowWeightStats[EventsInDifferentSentencesFeatureVector] {
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(event:Event) = { // featuresWithStats with next event
      if (event.present) {
        val otherSentences = for((otherSentence:Sentence) <- event.clue.sentence.doc ; if (otherSentence.position > event.clue.sentence.position))
        yield otherSentence
        for(sentence <- otherSentences ; (other:Event) <- sentence.events ; if (other.present)) yield Factor(event,other)
      }
      else Nil
    }
    def unroll2(event:Event) = { // featuresWithStats with prior event
      if (event.present) {
        //val allEventsInDoc = event
        val otherSentences = for((otherSentence:Sentence) <- event.clue.sentence.doc ; if (otherSentence.position < event.clue.sentence.position))
        yield otherSentence
        for(sentence <- otherSentences ; (other:Event) <- sentence.events ; if (other.present)) yield Factor(event,other)
      }
      else Nil    }
    def statistics(e1:Event, e2:Event):Stat = {
        Stat(new EventsInDifferentSentencesFeatureVector(e1,e2))
    }
  }
}