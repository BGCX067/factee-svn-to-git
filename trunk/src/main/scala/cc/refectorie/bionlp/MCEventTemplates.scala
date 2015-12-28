package cc.refectorie.bionlp

import cc.factorie.{DotStatistics1, Template2, BinaryVectorVariable}
import cc.refectorie.bionlp._
import cc.refectorie.bionlp.TemplatesForPieceWise.AllowWeightStats
import collection.mutable.{HashMap}
import util.Sorting

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object MCEventTemplates {


  class MCFeatureVector(e1:Event,e2:Event) extends BinaryVectorVariable[String] {
    // we now know, that s1 and s2 have the same string
    if (e1.clue.stringrepr != e2.clue.stringrepr) throw new Exception("Something went wrong!")

    val e1stem = e1.clue.map(t=>t.stem).mkString(" ")

    // transition of event type
    this += "EVENT-TRANS: "+e1.eventType.value+"-->"+e2.eventType.value
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+"Regulation"+"-->"+e2.eventType.value
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+e1.eventType.value+"-->"+"Regulation"
    this += "EVENT-TRANS: "+e1.eventType.value+"-->"+e2.eventType.value+":"+e1stem
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+"Regulation"+"-->"+e2.eventType.value+":"+e1stem
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+e1.eventType.value+"-->"+"Regulation"+":"+e1stem
    // debug
//    System.err.println(e1+"---"+e2)
//    this.values.foreach(v => System.err.println(v))
  }
  class MCEventsTemplate extends Template2[Event,Event] with DotStatistics1[MCFeatureVector] {
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(e1:Event) = {
      if (e1.present)
        for(o <- nextEventsWithSameClueString(e1) ; if (o != null && o.present)) yield Factor(e1,o)
      else Nil
    }
    def unroll2(e2:Event) = {
      if (e2.present)
        for(o <- previousEventsWithSameClueString(e2) ; if (o != null && o.present)) yield Factor(o,e2)
      else Nil
    }
    def statistics(e1:Event, e2:Event):Stat = {
        Stat(new MCFeatureVector(e1,e2))
    }
  }


  class ClueHasSameEventTypeFeatureVector(e1:Event,e2:Event) extends BinaryVectorVariable[String] {
    // we now know, that s1 and s2 have the same string
    if (e1.clue.stringrepr != e2.clue.stringrepr) throw new Exception("Something went wrong!")

    val e1stem = e1.clue.map(t=>t.stem).mkString(" ")

    // transition of event type
    if (e1.eventType.value == e2.eventType.value) {
      ///System.err.println(e1+":"+e2)
      this += "SAME-EVENTTYPE"
    }
  }
  class ClueHasSameEventTypeTemplate extends Template2[Event,Event] with DotStatistics1[ClueHasSameEventTypeFeatureVector] {
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(e1:Event) = {
      if (e1.present)
        for(o <- nextEventsWithSameClueString(e1) ; if (o != null && o.present)) yield Factor(e1,o)
      else Nil
    }
    def unroll2(e2:Event) = {
      if (e2.present)
        for(o <- previousEventsWithSameClueString(e2) ; if (o != null && o.present)) yield Factor(o,e2)
      else Nil
    }
    def statistics(e1:Event, e2:Event):Stat = {
        Stat(new ClueHasSameEventTypeFeatureVector(e1,e2))
    }
  }




  class MCArgRecFeatureVector(a1:Argument,a2:Argument) extends BinaryVectorVariable[String] {
    // we now know, that s1 and s2 have the same string
    if (a1.entity.span.stringrepr != a2.entity.span.stringrepr) throw new Exception("Something went wrong!")

    val argString = if (a1.entity.isInstanceOf[Gene]) "Gene" else a1.entity.span.map(t=>t.stem).mkString(" ")
    val e1 = a1.eventWithThisArgument
    val e2 = a2.eventWithThisArgument
    val e1stem = e1.clue.map(t=>t.stem).mkString("")
    val e2stem = e2.clue.map(t=>t.stem).mkString("")

    // transition of event type

    this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+e2.eventType.value
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+"Regulation"+"-->"+e2.eventType.value
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+"Regulation"
    this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+e2.eventType.value+":"+argString
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+"Regulation"+"-->"+e2.eventType.value+":"+argString
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+"Regulation"+":"+argString

    this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+e2.eventType.value+a1.roleType.value+a2.roleType.value
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+"Regulation"+"-->"+e2.eventType.value+a1.roleType.value+a2.roleType.value
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+"Regulation"+a1.roleType.value+a2.roleType.value
    this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+e2.eventType.value+":"+argString+a1.roleType.value+a2.roleType.value
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+"Regulation"+"-->"+e2.eventType.value+":"+argString+a1.roleType.value+a2.roleType.value
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+"Regulation"+":"+argString+a1.roleType.value+a2.roleType.value

    this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+e2.eventType.value+a1.roleType.value+a2.roleType.value+e1stem+e2stem
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+"Regulation"+"-->"+e2.eventType.value+a1.roleType.value+a2.roleType.value+e1stem+e2stem
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+"Regulation"+a1.roleType.value+a2.roleType.value+e1stem+e2stem
    this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+e2.eventType.value+":"+argString+a1.roleType.value+a2.roleType.value+e1stem+e2stem
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+"Regulation"+"-->"+e2.eventType.value+":"+argString+a1.roleType.value+a2.roleType.value+e1stem+e2stem
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS-REC: "+e1.eventType.value+"-->"+"Regulation"+":"+argString+a1.roleType.value+a2.roleType.value+e1stem+e2stem

    // debug
//    System.err.println(e1+"---"+e2)
//    this.values.foreach(v => System.err.println(v))
  }
  class MCArgumentRecTemplate extends Template2[Argument,Argument] with DotStatistics1[MCArgRecFeatureVector] {
    System.err.println("Init MCArgumentTemplate")
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(a1:Argument) = {
      if (a1.present && a1.eventWithThisArgument.present) {
        // search for an event on level2, which has the same argument
        val argCandidateSpans = nextSpansWithSameString(a1.entity.span,false)

        val candidate = argCandidateSpans.find(s => {
          val allEvents = s.sentence.events
          allEvents.exists(e1 => {
            e1.eventType.value == a1.eventWithThisArgument.eventType.value && // same eventtype
            e1.arguments.exists(a => {
              a.entity.isInstanceOf[Event] &&
                      a.entity.asInstanceOf[Event].arguments.exists(aa=>{aa.entity.span==s})}) // has arg which is event and has span as arg
          })
        })

        // we have it, get it
        if (candidate.isDefined) {
          val e2e3 =
          for(eLevel2 <- candidate.get.sentence.events ; eLevel1 <- eLevel2.arguments.filter(x=>x.entity.isInstanceOf[Event]) ;
              if (eLevel1.entity.asInstanceOf[Event].arguments.exists(x=>x.entity.span==candidate.get) && eLevel2.eventType.value==a1.eventWithThisArgument.eventType.value))
               yield (eLevel1.entity.asInstanceOf[Event],eLevel2)
//          for((e2,e3) <- e2e3) {
//            System.err.println(a1.eventWithThisArgument.outputFormatE.trim+" "+a1.eventWithThisArgument.outputFormatT.trim)
//            System.err.println(a1.entity.genericOutputFormatT.trim)
//            val x = e2.arguments.find(a=>a.entity.span==candidate.get)
//            System.err.println(e2.outputFormatE.trim+" "+e2.outputFormatT.trim)
//            System.err.println(e3.outputFormatE.trim+" "+e3.outputFormatT.trim)
//            System.err.println(x.get.entity.genericOutputFormatT.trim)
//            System.err.println("-----\n")
//          }
          for((e2,e3) <- e2e3) yield Factor(a1,e2.arguments.find(a=>a.entity.span==candidate.get).get)
        } else Nil
      }
      else Nil
    }
    def unroll2(a2:Argument) = {
      if (a2.present && a2.eventWithThisArgument.present) {
        // search for an event on level2, which has the same argument
        val argCandidateSpans = previousSpansWithSameString(a2.entity.span,false)

        val candidate = argCandidateSpans.find(s => {
          val allEvents = s.sentence.events
          allEvents.exists(e1 => {
            e1.eventType.value == a2.eventWithThisArgument.eventType.value && // same eventtype
            e1.arguments.exists(a => {
              a.entity.isInstanceOf[Event] &&
                      a.entity.asInstanceOf[Event].arguments.exists(aa=>{aa.entity.span==s})}) // has arg which is event and has span as arg
          })
        })

        // we have it, get it
        if (candidate.isDefined) {
          val e2e3 =
          for(eLevel2 <- candidate.get.sentence.events ; eLevel1 <- eLevel2.arguments.filter(x=>x.entity.isInstanceOf[Event]) ;
              if (eLevel1.entity.asInstanceOf[Event].arguments.exists(x=>x.entity.span==candidate.get) && eLevel2.eventType.value==a2.eventWithThisArgument.eventType.value))
               yield (eLevel1.entity.asInstanceOf[Event],eLevel2)
//          for((e2,e3) <- e2e3) {
//            System.err.println(a1.eventWithThisArgument.outputFormatE.trim+" "+a1.eventWithThisArgument.outputFormatT.trim)
//            System.err.println(a1.entity.genericOutputFormatT.trim)
//            val x = e2.arguments.find(a=>a.entity.span==candidate.get)
//            System.err.println(e2.outputFormatE.trim+" "+e2.outputFormatT.trim)
//            System.err.println(e3.outputFormatE.trim+" "+e3.outputFormatT.trim)
//            System.err.println(x.get.entity.genericOutputFormatT.trim)
//            System.err.println("-----\n")
//          }
          for((e2,e3) <- e2e3) yield Factor(e2.arguments.find(a=>a.entity.span==candidate.get).get,a2)
        } else Nil
      }
      else Nil
    }
    def statistics(a1:Argument, a2:Argument):Stat = {
      Stat(new MCArgRecFeatureVector(a1,a2))
    }
  }





  class MCArgFeatureVector(a1:Argument,a2:Argument) extends BinaryVectorVariable[String] {
    // we now know, that s1 and s2 have the same string
    if (a1.entity.span.stringrepr != a2.entity.span.stringrepr) throw new Exception("Something went wrong!")

    val argString = if (a1.entity.isInstanceOf[Gene]) "Gene" else a1.entity.span.map(t=>t.stem).mkString(" ")
    val e1 = a1.eventWithThisArgument
    val e2 = a2.eventWithThisArgument

    // transition of event type

    this += "EVENT-TRANS: "+e1.eventType.value+"-->"+e2.eventType.value
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+"Regulation"+"-->"+e2.eventType.value
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+e1.eventType.value+"-->"+"Regulation"
    this += "EVENT-TRANS: "+e1.eventType.value+"-->"+e2.eventType.value+":"+argString
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+"Regulation"+"-->"+e2.eventType.value+":"+argString
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+e1.eventType.value+"-->"+"Regulation"+":"+argString

    this += "EVENT-TRANS: "+e1.eventType.value+"-->"+e2.eventType.value+a1.roleType.value+a2.roleType.value
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+"Regulation"+"-->"+e2.eventType.value+a1.roleType.value+a2.roleType.value
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+e1.eventType.value+"-->"+"Regulation"+a1.roleType.value+a2.roleType.value
    this += "EVENT-TRANS: "+e1.eventType.value+"-->"+e2.eventType.value+":"+argString+a1.roleType.value+a2.roleType.value
    if (e1.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+"Regulation"+"-->"+e2.eventType.value+":"+argString+a1.roleType.value+a2.roleType.value
    if (e2.eventType.value.contains("egulation")) this += "EVENT-TRANS: "+e1.eventType.value+"-->"+"Regulation"+":"+argString+a1.roleType.value+a2.roleType.value

    // debug
//    System.err.println(e1+"---"+e2)
//    this.values.foreach(v => System.err.println(v))
  }
  class MCArgumentTemplate extends Template2[Argument,Argument] with DotStatistics1[MCArgFeatureVector] {
    System.err.println("Init MCArgumentTemplate")
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(a1:Argument) = {
      if (a1.present && a1.eventWithThisArgument.present) {
        // search for the next event which has an argument with the same string as a1
        val argCandidateSpans = nextSpansWithSameString(a1.entity.span,false)
        // find the first one which is used as an argument
        val firstRelevantSpan = argCandidateSpans.find(s => s.sentence.events.exists(e=>e.arguments.exists(a=>a.entity.span==s)))
        // as which argument object?
        if (firstRelevantSpan.isDefined) {
          val firstArgs = for(e <- firstRelevantSpan.get.sentence.events ; a <- e.arguments ; if (a.entity.span == firstRelevantSpan.get)) yield a
          for(a <- firstArgs) yield Factor(a1,a)
        } else Nil
      }
      else Nil
    }
    def unroll2(a2:Argument) = {
      if (a2.present && a2.eventWithThisArgument.present) {
        // search for the next event which has an argument with the same string as a1
        val argCandidateSpans = previousSpansWithSameString(a2.entity.span,false)
        // find the first one which is used as an argument
        val firstRelevantSpan = argCandidateSpans.find(s => s.sentence.events.exists(e=>e.arguments.exists(a=>a.entity.span==s)))
        // as which argument object?
        if (firstRelevantSpan.isDefined) {
          val firstArgs = for(e <- firstRelevantSpan.get.sentence.events ; a <- e.arguments ; if (a.entity.span == firstRelevantSpan)) yield a
          for(a <- firstArgs) yield Factor(a,a2)
        } else Nil
      }
      else Nil    }
    def statistics(a1:Argument, a2:Argument):Stat = {
      Stat(new MCArgFeatureVector(a1,a2))
    }
  }

  // should be done once at beginning to be faster
  def nextSpansWithSameString(currentSpan:Span, useTrueSentence:Boolean) : Seq[Span] = {
    val allSucceedingsSpans =
    if (useTrueSentence) {
    for(sentenceInDoc:Sentence <- currentSpan.sentence.doc ; succeedingsSpan:Span <- sentenceInDoc.trueSentence.spans ;
        if ((sentenceInDoc.start > currentSpan.sentence.start) ||
                (sentenceInDoc.start == currentSpan.sentence.start && succeedingsSpan.start > currentSpan.start)) &&
             currentSpan.stringrepr == succeedingsSpan.stringrepr)
      yield succeedingsSpan
    } else {
      for(sentenceInDoc:Sentence <- currentSpan.sentence.doc ; succeedingsSpan:Span <- sentenceInDoc.spans ;
          if ((sentenceInDoc.start > currentSpan.sentence.start) ||
                  (sentenceInDoc.start == currentSpan.sentence.start && succeedingsSpan.start > currentSpan.start)) &&
                  currentSpan.stringrepr == succeedingsSpan.stringrepr)
      yield succeedingsSpan
    }

    if (allSucceedingsSpans.size > 1) {
      Sorting.stableSort(allSucceedingsSpans,(s1:Span,s2:Span)=>{(s1.sentence.start < s2.sentence.start || (s1.sentence.start == s2.sentence.start && s1.start < s2.start))})
    } else
      allSucceedingsSpans // empty or just one element
  }
  def nextEventsWithSameClueString(currentEvent:Event): Seq[Event] = { // returns list of events on the succeeding span with same string
    val nextSpans:Seq[Span] = nextSpansWithSameString(currentEvent.clue,false)
    val spanWithEvent = nextSpans.find(s => s.events.size > 0)
    if (spanWithEvent.isDefined)
      spanWithEvent.get.events
    else Nil
  }

  // should be done once at beginning to be faster
  def previousSpansWithSameString(currentSpan:Span, useTrueSentence:Boolean) : Seq[Span] = {
    val allPrecedingSpans =
    if (useTrueSentence) {
    for(sentenceInDoc:Sentence <- currentSpan.sentence.doc ; precedingSpan:Span <- sentenceInDoc.trueSentence.spans ;
        if ((sentenceInDoc.start < currentSpan.sentence.start) ||
                (sentenceInDoc.start == currentSpan.sentence.start && precedingSpan.start < currentSpan.start)) &&
             currentSpan.stringrepr == precedingSpan.stringrepr)
      yield precedingSpan
    } else {
      for(sentenceInDoc:Sentence <- currentSpan.sentence.doc ; precedingSpan:Span <- sentenceInDoc.spans ;
        if ((sentenceInDoc.start < currentSpan.sentence.start) ||
                (sentenceInDoc.start == currentSpan.sentence.start && precedingSpan.start < currentSpan.start)) &&
             currentSpan.stringrepr == precedingSpan.stringrepr)
      yield precedingSpan
    }

    if (allPrecedingSpans.size > 1) { // sorted backwards, the first is the last!
      Sorting.stableSort(allPrecedingSpans,(s1:Span,s2:Span)=>{(s1.sentence.start > s2.sentence.start || (s1.sentence.start == s2.sentence.start && s1.start > s2.start))})
    } else
      allPrecedingSpans // empty or just one element
  }
  def previousEventsWithSameClueString(currentEvent:Event): Seq[Event] = { // returns list of events on the previous span with same string
    val previousSpans:Seq[Span] = previousSpansWithSameString(currentEvent.clue,false) // this is sorting backwards
    val spanWithEvent = previousSpans.find(s => s.events.size > 0) // hence, first is last
    if (spanWithEvent.isDefined)
      spanWithEvent.get.events
    else
      Nil
  }

  def main(args:Array[String]) :Unit = {
    val c = Data.readCorpus("data/corpora/sample")
    val d = c.first
    val f = d.first.trueSentence.events.first
    System.err.println(f)
    val g = nextEventsWithSameClueString(f)
    System.err.println(g)
  }
}