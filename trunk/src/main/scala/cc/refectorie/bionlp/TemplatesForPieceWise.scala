package cc.refectorie.bionlp

import cc.factorie._
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object TemplatesForPieceWise {


  trait AllowWeightStats[A <: DiscreteValues] extends DotStatistics1[A] {
    def weightStat : (Double,Double,Double) = {
      var min:Double = 999999999.0
      var max:Double = -999999999.0
      var mean:Double = 0.0
      for(w:Double <- this.weights.values) {
        max = if (max < w) w else max
        min = if (min > w) w else min
        mean += w
      }
      mean /= this.weights.size
      (min,max,mean)
    }
  }

  //
  class EventEventSharingArgFeatureVector(e1:Event,e2:Event) extends BinaryVectorVariable[String] {
    // path between this events
    for(t1 <- e1.clue.spannedTokens ; t2 <- e2.clue.spannedTokens) {
      val path:Iterable[String] = if (InitFile.selftokenizedpath)
        StanfordDependencies.featureFromPath(t1,t2,null,null,false,"Event-Event-")
      else if (InitFile.mcpath)
        StanfordDependencies.featureFromPath(t1,t2,null,null,true,"Event-Event-")
      else Nil
      path.foreach(x => {this += x ; this += e1.eventType.value+"-"+e2.eventType.value+"-"+x})
    }
    // for each argument shared between both, paths to both events
    for(arg <- e1.arguments ; if (e2.containsEntityAsArgumentNonDeep(arg.entity))) {
      for(t1 <- e1.clue.spannedTokens ; t2 <- arg.entity.span.spannedTokens) {
        val path:Iterable[String] = if (InitFile.selftokenizedpath)
          StanfordDependencies.featureFromPath(t1,t2,null,null,false,"Event1-ARG-")
        else if (InitFile.mcpath)
          StanfordDependencies.featureFromPath(t1,t2,null,null,true,"Event1-ARG-")
        else Nil
        path.foreach(x => {this += x ; this += e1.eventType.value+"-"+arg.roleType.value+"-"+x})
      }
      for(t1 <- e2.clue.spannedTokens ; t2 <- arg.entity.span.spannedTokens) {
        val path:Iterable[String] = if (InitFile.selftokenizedpath)
          StanfordDependencies.featureFromPath(t1,t2,null,null,false,"Event2-ARG-")
        else if (InitFile.mcpath)
          StanfordDependencies.featureFromPath(t1,t2,null,null,true,"Event2-ARG-")
        else Nil
        path.foreach(x => {this += x ; this += e2.eventType.value+"-"+arg.roleType.value+"-"+x})
      }
    }
    // could this be a coordination?
    if (Data.isSymbolBetweenTokens("and",e1.clue.first,e2.clue.first) || Data.isSymbolBetweenTokens("as",e1.clue.first,e2.clue.first)) {
      this += "COORD"
    }
    //debug
//    System.err.println(e1.clue.sentence.toString(true))
//    for(x <- this.values) {
//      System.err.println(x)
//    }
//    System.err.println("--------------------------------------")
  }

  class EventEventSharingArgTemplate extends Template2[Event,Event] with DotStatistics1[EventEventSharingArgFeatureVector] with AllowWeightStats[EventEventSharingArgFeatureVector] {
    val featureVectorCache = new HashMap[String,EventEventSharingArgFeatureVector]
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(event:Event) = {
      if (event.present) {
        for((other:Event) <- event.clue.sentence.events ; if (other.clue.start < event.clue.start && other.present && event.arguments.exists(a => other.containsEntityAsArgumentNonDeep(a.entity))))
        yield Factor(event,other)
      }
      else Nil
    }
    def unroll2(event:Event) = {
      if (event.present)
        for((other:Event) <- event.clue.sentence.events ; if (other.clue.start > event.clue.start && other.present && event.arguments.exists(a => other.containsEntityAsArgumentNonDeep(a.entity))))
        yield Factor(event,other)
      else
        Nil
    }
    def statistics(e1:Event, e2:Event):Stat = {
      val h = e1.hash+"->"+e2.hash
      if ((InitFile.featurecaching) && featureVectorCache.contains(h))
        Stat(featureVectorCache(h))
      else {
        val fv = new EventEventSharingArgFeatureVector(e1,e2)
        if (InitFile.featurecaching) featureVectorCache += h -> fv
        Stat(fv)
      }
    }
  }
  //


  //
  class SimilarEventDifferentArgFeatureVector(e1:Event,e2:Event) extends BinaryVectorVariable[String] {
    for((a1:Argument) <-e1.arguments ; (a2:Argument) <-e2.arguments ; if (a1.roleType.value == a2.roleType.value)) {
      for(t1 <- a1.entity.span.spannedTokens ; t2 <- a1.entity.span.spannedTokens) {
        val path:Iterable[String] = if (InitFile.selftokenizedpath)
          StanfordDependencies.featureFromPath(t1,t2,null,null,false,"Arg-Arg-")
        else if (InitFile.mcpath)
          StanfordDependencies.featureFromPath(t1,t2,null,null,true,"Arg-Arg-")
        else Nil
        path.foreach(x => {this += x ; this += e1.eventType.value+":"+// eventtypes are the same of both events
          (if (a1.entity.isInstanceOf[Event]) a1.entity.asInstanceOf[Event].eventType.value else "gene")+":"+
                  (if (a2.entity.isInstanceOf[Event]) a2.entity.asInstanceOf[Event].eventType.value else "gene")+
                  "-"+x})
      }
      if (Data.isSymbolBetweenTokens("and",a1.entity.span.first,a2.entity.span.first) || Data.isSymbolBetweenTokens("as",a1.entity.span.first,a2.entity.span.first)) {
        this += "ARG-COORD"
      }
    }
    // debug
//    System.err.println(e1.clue.sentence.toString(true))
//    for(x <- this.values) {
//      System.err.println(x)
//    }
//    System.err.println("--------------------------------------")
  }
  class SimilarEventDifferentArgTemplate extends Template2[Event,Event] with DotStatistics1[SimilarEventDifferentArgFeatureVector] with AllowWeightStats[SimilarEventDifferentArgFeatureVector] {
    val featureVectorCache = new HashMap[String,SimilarEventDifferentArgFeatureVector]
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(event:Event) = {
      if (event.present) {
        for((other:Event) <- event.clue.sentence.events ; if (other.clue.start == event.clue.start && other.present && other.eventType.value == event.eventType.value))
        yield Factor(event,other)
      }
      else Nil
    }
    def unroll2(event:Event) = {
       Nil
    }
    def statistics(e1:Event, e2:Event):Stat = {
      val h = e1.hash+"->"+e2.hash
      if ((InitFile.featurecaching) && featureVectorCache.contains(h))
        Stat(featureVectorCache(h))
      else {
        val fv = new SimilarEventDifferentArgFeatureVector(e1,e2)
        if (InitFile.featurecaching) featureVectorCache += h -> fv
        Stat(fv)
      }
    }
  }
  //


  class EventFeatureVector(event:Event) extends BinaryVectorVariable[String] {
    val stringrepr = event.clue.map(_.word).mkString(" ")
    val stems = event.clue.map(_.stem).mkString(" ")
    val sentence = event.clue.sentence
    val sentencePos = event.clue.start
    val clueLength = event.clue.length
    val kindOfRegulation = InitFile.generalizePosNegRegulation && event.eventType.value.contains("egulation")

    // clue based
    this += "DEFAULT"
    this += "TYPE="+event.eventType.value
    this += "WORD-AND-TYPE="+event.eventType.value + "->" + stems
    if (kindOfRegulation) {
      this += "TYPE="+"Regulation"
      this += "WORD-AND-TYPE="+"Regulation"+ "->" + stems
    }
    this += "STRING="+stringrepr
    this += "STEMS="+stems

    // general
    this += "NUM-OF-ARGS="+event.arguments.length
    this += "NUM-OF-THEMES="+event.getThemes.length

    // theme based
    for(theme:Argument <- event.getThemes ; if (!(InitFile.nootherfeaturesforselfregulation && event.clue == theme.entity.span))) {

      val themeType = if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"
      val themeString = if (theme.entity.isInstanceOf[Gene]) "Gene" else theme.entity.span.map(_.word).mkString(" ")

      if (theme.entity.isInstanceOf[Event])
        this += "THEMESTEM="+theme.entity.span.map(_.stem).mkString(" ")
      if (theme.entity.isInstanceOf[Gene]) this += "THEME-IS-GENE"
      if (theme.entity.isInstanceOf[Event]) this += "THEME-IS-EVENT"
      if (theme.entity.isInstanceOf[Gene]) this += "THEME-IS-GENE,THISEVENTTYPE="+event.eventType.value
      if (theme.entity.isInstanceOf[Gene] && kindOfRegulation) this += "THEME-IS-GENE,THISEVENTTYPE="+"Regulation"
      if (theme.entity.isInstanceOf[Event]) this += "THEME-IS-EVENT,THISEVENTTYPE="+event.eventType.value
      if (theme.entity.isInstanceOf[Event] && kindOfRegulation) this += "THEME-IS-EVENT,THISEVENTTYPE="+"Regulation"
      this += "CLUE-THEME-TYPE-STEM"+stems+"---"+(if (theme.entity.isInstanceOf[Gene]) "Gene" else theme.entity.span.map(_.stem).mkString(" "))+"---"+event.eventType.value
      if (kindOfRegulation)
        this += "CLUE-THEME-TYPE-STEM"+stems+"---"+(if (theme.entity.isInstanceOf[Gene]) "Gene" else theme.entity.span.map(_.stem).mkString(" "))+"---"+"Regulation"
      this += "CLUE-THEME-STEM"+stems+"---"+(if (theme.entity.isInstanceOf[Gene]) "Gene" else theme.entity.span.map(_.stem).mkString(" "))
      this += "CLUE-THEME-TYPE-STRING"+stringrepr+"---"+themeString+"---"+event.eventType.value
      if (kindOfRegulation) this += "CLUE-THEME-TYPE-STRING"+stringrepr+"---"+themeString+"---"+"Regulation"
      this += "CLUE-THEME-STRING"+stringrepr+"---"+themeString
      this += "THISTYPE-THEMETYPE="+event.eventType.value+"---"+themeType
      if (kindOfRegulation)
        this += "THISTYPE-THEMETYPE="+"Regulation"+"---"+themeType
      if (InitFile.miGene && theme.entity.span.stringrepr == event.clue.sentence.doc.mostImportantGene) {
        if (theme.entity.isInstanceOf[Gene]) this += "THEME-IS-GENE"+"MIG"
        if (theme.entity.isInstanceOf[Gene]) this += "THEME-IS-GENE,THISEVENTTYPE="+event.eventType.value+"MIG"
        if (theme.entity.isInstanceOf[Gene] && kindOfRegulation) this += "THEME-IS-GENE,THISEVENTTYPE="+"Regulation"+"MIG"
      }
      
      for(t1 <- event.clue.spannedTokens ; t2 <- theme.entity.span.spannedTokens) {
        val path:Iterable[String] = if (InitFile.selftokenizedpath)
          StanfordDependencies.featureFromPath(t1,t2,InitFile.pathngrams,InitFile.pathnprefix,false,"THEME-CLUE-")
        else if (InitFile.mcpath)
          StanfordDependencies.featureFromPath(t1,t2,InitFile.pathngrams,InitFile.pathnprefix,true,"THEME-CLUE-")
        else Nil
        path.foreach(x => {this += x+"_WITH_TYPE_"+event.eventType.value}+"-TO-"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene")))
        path.foreach(x => {this += x+"_WITH_TYPE_"+event.eventType.value}+"-TO-"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))+"="+theme.entity.span.stringrepr)
        if (kindOfRegulation) {
          path.foreach(x => {this += x+"_WITH_TYPE_"+"Regulation"}+"-TO-"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene")))
          path.foreach(x => {this += x+"_WITH_TYPE_"+"Regulation"}+"-TO-"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))+"="+theme.entity.span.stringrepr)
        }
        if (path.isEmpty) this += "THEME-CLUE-NO-PATH"
      }

      // self regulation
      if (event.clue.start == theme.entity.span.start) {
        this += "EventArgClueIsEqual"
        this += "EventArgClueIsEqual:"+event.eventType.value+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))
        this += "EventArgClueIsEqual:"+event.eventType.value+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))+stringrepr
        if (kindOfRegulation) {
          this += "EventArgClueIsEqual:"+"Regulation"+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))
          this += "EventArgClueIsEqual:"+"Regulation"+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))+stringrepr
        }
      }

      // text order
      if (event.clue.start < theme.entity.span.start) {
        this += "EVENT-BEFORE-THEME:"+event.eventType.value+":"+stringrepr+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))
        this += "EVENT-BEFORE-THEME:"+event.eventType.value+":"+stringrepr+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))+":"+(if (theme.entity.isInstanceOf[Event]) theme.entity.span.stringrepr else "gene")
        if (kindOfRegulation) {
          this += "EVENT-BEFORE-THEME:"+"Regulation"+":"+stringrepr+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))
          this += "EVENT-BEFORE-THEME:"+"Regulation"+":"+stringrepr+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))+":"+(if (theme.entity.isInstanceOf[Event]) theme.entity.span.stringrepr else "gene")
        }
      }
      if (event.clue.start > theme.entity.span.start) {
        this += "EVENT-AFTER-THEME:"+event.eventType.value+":"+stringrepr+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))
        this += "EVENT-AFTER-THEME:"+event.eventType.value+":"+stringrepr+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))+":"+(if (theme.entity.isInstanceOf[Event]) theme.entity.span.stringrepr else "gene")
        if (kindOfRegulation) {
          this += "EVENT-AFTER-THEME:"+"Regulation"+":"+stringrepr+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))
          this += "EVENT-AFTER-THEME:"+"Regulation"+":"+stringrepr+":"+((if (theme.entity.isInstanceOf[Event]) theme.entity.asInstanceOf[Event].eventType.value else "gene"))+":"+(if (theme.entity.isInstanceOf[Event]) theme.entity.span.stringrepr else "gene")          
        }
      }

      // distance
      val dist = (event.clue.start - theme.entity.span.start)
      if (Math.abs(dist) > 15) this += "THEMEDISTANCE>15"
      if (Math.abs(dist) > 20) this += "THEMEDISTANCE>20"
      if (Math.abs(dist) > 25) this += "THEMEDISTANCE>25"

      // very close featuresWithStats
      if (InitFile.veryCloseArgsFeature && Math.abs(dist) < 5) {
//        this += "DIST-STRING-AND-TYPE"+event.eventType.value+":"+stringrepr+":"+themeType+"->"+dist
        this += "DIST-AND-TYPE"+event.eventType.value+":"+themeType+"->"+dist
        if (kindOfRegulation)
          this += "DIST-AND-TYPE"+"Regulation"+":"+themeType+"->"+dist
      }
      if (InitFile.veryCloseArgsFeature2 && Math.abs(dist) < 5) {
        val textBetweenClueAndArg = sentence.filter(t => (t.sentencePos > event.clue.last.sentencePos && t.sentencePos < theme.entity.span.first.sentencePos) ||
          (t.sentencePos < event.clue.first.sentencePos && t.sentencePos > theme.entity.span.last.sentencePos)).map(t => t.word).mkString(" ")
        this += "TextToTheme="+textBetweenClueAndArg
        this += "TypeTextToTheme="+event.eventType.value+":"+textBetweenClueAndArg
      }

      if (InitFile.headfeature) {
        for(t <- theme.entity.span ; if (StanfordDependencies.tokenToDepBackward.contains(t))) {
          for(t2 <- StanfordDependencies.tokenToDepBackward(t).keySet) {
            this += "THEMEHEAD="+StanfordDependencies.tokenToDepBackward(t)(t2).dependencyType
            this += "THEMEHEAD="+StanfordDependencies.tokenToDepBackward(t)(t2).dependencyType+":"+t2.stem
            this += "THEMEHEAD="+t2.stem
          }
        }
        if (theme.entity.span.first.sentencePos > 0) this += "THEMELEFTNEIGHBOR1="+sentence(theme.entity.span.first.sentencePos-1).stem
        if (theme.entity.span.last.sentencePos < sentence.length-1) this += "THEMERIGHTNEIGHBOR1="+sentence(theme.entity.span.last.sentencePos+1).stem
        if (theme.entity.span.first.sentencePos > 1) this += "THEMELEFTNEIGHBOR2="+sentence(theme.entity.span.first.sentencePos-2).stem
        if (theme.entity.span.last.sentencePos < sentence.length-2) this += "THEMERIGHTNEIGHBOR2="+sentence(theme.entity.span.last.sentencePos+2).stem
      }
    }
    

    // cause based
    for(cause:Argument <- event.arguments.filter(a => !event.getThemes.contains(a))) { // for all causes (every arg which is not theme)
      val causeType = if (cause.entity.isInstanceOf[Event]) cause.entity.asInstanceOf[Event].eventType.value else "gene"
      if (cause.entity.isInstanceOf[Event])
        this += "CAUSESTEM="+cause.entity.span.map(_.stem).mkString(" ")
      if (cause.entity.isInstanceOf[Gene]) this += "CAUSE-IS-GENE"
      if (cause.entity.isInstanceOf[Event]) this += "CAUSE-IS-EVENT"
      if (cause.entity.isInstanceOf[Gene]) this += "CAUSE-IS-GENE,THISEVENTTYPE="+event.eventType.value
      if (cause.entity.isInstanceOf[Event]) this += "CAUSE-IS-EVENT,THISEVENTTYPE="+event.eventType.value
      if (kindOfRegulation) {
        if (cause.entity.isInstanceOf[Gene]) this += "CAUSE-IS-GENE,THISEVENTTYPE="+"Regulation"
        if (cause.entity.isInstanceOf[Event]) this += "CAUSE-IS-EVENT,THISEVENTTYPE="+"Regulation"  
      }
      this += "CLUE-CAUSE-STEM="+stems+"---"+(if (cause.entity.isInstanceOf[Gene]) "Gene" else cause.entity.span.map(_.stem).mkString(" "))
      this += "CLUE-CAUSE-TYPE-STEM="+stems+"---"+(if (cause.entity.isInstanceOf[Gene]) "Gene" else cause.entity.span.map(_.stem).mkString(" "))+"---"+event.eventType.value
      if (kindOfRegulation)
        this += "CLUE-CAUSE-TYPE-STEM="+stems+"---"+(if (cause.entity.isInstanceOf[Gene]) "Gene" else cause.entity.span.map(_.stem).mkString(" "))+"---"+"Regulation"
      this += "CLUE-CAUSE-STRING="+stringrepr+"---"+(if (cause.entity.isInstanceOf[Gene]) "Gene" else cause.entity.span.map(_.word).mkString(" "))
      this += "CLUE-CAUSE-TYPE-STRING="+stringrepr+"---"+(if (cause.entity.isInstanceOf[Gene]) "Gene" else cause.entity.span.map(_.word).mkString(" "))+"---"+event.eventType.value
      if (kindOfRegulation)
        this += "CLUE-CAUSE-TYPE-STRING="+stringrepr+"---"+(if (cause.entity.isInstanceOf[Gene]) "Gene" else cause.entity.span.map(_.word).mkString(" "))+"---"+"Regulation"
      this += "THISTYPE-CAUSETYPE="+event.eventType.value+"---"+(if (cause.entity.isInstanceOf[Event]) cause.entity.asInstanceOf[Event].eventType.value else "gene")
      if (InitFile.miGene && cause.entity.span.stringrepr == event.clue.sentence.doc.mostImportantGene) {
        if (cause.entity.isInstanceOf[Gene]) this += "CAUSE-IS-GENE"+"MIG"
        if (cause.entity.isInstanceOf[Gene]) this += "CAUSE-IS-GENE,THISEVENTTYPE="+event.eventType.value+"MIG"
        if (cause.entity.isInstanceOf[Gene] && kindOfRegulation) this += "CAUSE-IS-GENE,THISEVENTTYPE="+"Regulation"+"MIG"
      }

      for(t1 <- event.clue.spannedTokens ; t2 <- cause.entity.span.spannedTokens) {
        val path:Iterable[String] = if (InitFile.selftokenizedpath)
          StanfordDependencies.featureFromPath(t1,t2,InitFile.pathngrams,InitFile.pathnprefix,false,"CAUSE-CLUE-")
        else if (InitFile.mcpath)
          StanfordDependencies.featureFromPath(t1,t2,InitFile.pathngrams,InitFile.pathnprefix,true,"CAUSE-CLUE-")
        else Nil
        path.foreach(x => {this += x+"_WITH_TYPE_"+event.eventType.value}+"-TO-"+((if (cause.entity.isInstanceOf[Event]) cause.entity.asInstanceOf[Event].eventType.value else "gene")))
        path.foreach(x => {this += x+"_WITH_TYPE_"+event.eventType.value}+"-TO-"+((if (cause.entity.isInstanceOf[Event]) cause.entity.asInstanceOf[Event].eventType.value else "gene"))+"="+cause.entity.span.stringrepr)
        if (kindOfRegulation) {
          path.foreach(x => {this += x+"_WITH_TYPE_"+"Regulation"}+"-TO-"+((if (cause.entity.isInstanceOf[Event]) cause.entity.asInstanceOf[Event].eventType.value else "gene")))
          path.foreach(x => {this += x+"_WITH_TYPE_"+"Regulation"}+"-TO-"+((if (cause.entity.isInstanceOf[Event]) cause.entity.asInstanceOf[Event].eventType.value else "gene"))+"="+cause.entity.span.stringrepr)  
        }
        if (path.isEmpty) this += "CAUSE-CLUE-NO-PATH"
      }

      // text order
      if (event.clue.start < cause.entity.span.start) {
        this += "EVENT-BEFORE-CAUSE:"+event.eventType.value+":"+stringrepr+":"+causeType
        this += "EVENT-BEFORE-CAUSE:"+event.eventType.value+":"+stringrepr+":"+causeType+":"+(if (cause.entity.isInstanceOf[Event]) cause.entity.span.stringrepr else "gene")
        if (kindOfRegulation) {
          this += "EVENT-BEFORE-CAUSE:"+"Regulation"+":"+stringrepr+":"+causeType
          this += "EVENT-BEFORE-CAUSE:"+"Regulation"+":"+stringrepr+":"+causeType+":"+(if (cause.entity.isInstanceOf[Event]) cause.entity.span.stringrepr else "gene")  
        }
      }
      if (event.clue.start > cause.entity.span.start) {
        this += "EVENT-AFTER-CAUSE:"+event.eventType.value+":"+stringrepr+":"+causeType
        this += "EVENT-AFTER-CAUSE:"+event.eventType.value+":"+stringrepr+":"+causeType+":"+(if (cause.entity.isInstanceOf[Event]) cause.entity.span.stringrepr else "gene")
        if (kindOfRegulation) {
          this += "EVENT-AFTER-CAUSE:"+"Regulation"+":"+stringrepr+":"+causeType
          this += "EVENT-AFTER-CAUSE:"+"Regulation"+":"+stringrepr+":"+causeType+":"+(if (cause.entity.isInstanceOf[Event]) cause.entity.span.stringrepr else "gene")  
        }
      }

      // distance
      val dist = event.clue.start - cause.entity.span.start
      if (Math.abs(dist) > 15) this += "CAUSEDISTANCE>15"
      if (Math.abs(dist) > 20) this += "CAUSEDISTANCE>20"
      if (Math.abs(dist) > 25) this += "CAUSEDISTANCE>25"

      // very close featuresWithStats

      if (InitFile.veryCloseArgsFeature && Math.abs(dist) < 5) {
//        this += "CAUSEDIST-STRING-AND-TYPE"+event.eventType.value+":"+stringrepr+":"+causeType+"->"+dist
//        this += "CAUSEDIST-AND-TYPE"+event.eventType.value+":"+causeType+"->"+dist
      }
      if (InitFile.veryCloseArgsFeature2 && Math.abs(dist) < 5) {
        val textBetweenClueAndArg = sentence.filter(t => (t.sentencePos > event.clue.last.sentencePos && t.sentencePos < cause.entity.span.first.sentencePos) ||
          (t.sentencePos < event.clue.first.sentencePos && t.sentencePos > cause.entity.span.last.sentencePos)).map(t => t.word).mkString(" ")
        this += "TextToCause="+textBetweenClueAndArg
        this += "TypeTextToCause="+event.eventType.value+":"+textBetweenClueAndArg
      }

      if (InitFile.headfeature) {
        for(t <- cause.entity.span ; if (StanfordDependencies.tokenToDepBackward.contains(t))) {
          for(t2 <- StanfordDependencies.tokenToDepBackward(t).keySet) {
            this += "CAUSEHEAD="+StanfordDependencies.tokenToDepBackward(t)(t2).dependencyType
            this += "CAUSEHEAD="+StanfordDependencies.tokenToDepBackward(t)(t2).dependencyType+":"+t2.stem
            this += "CAUSEHEAD="+t2.stem
          }
        }
        if (cause.entity.span.first.sentencePos > 0) this += "CAUSELEFTNEIGHBOR1="+sentence(cause.entity.span.first.sentencePos-1).stem
        if (cause.entity.span.last.sentencePos < sentence.length-1) this += "CAUSERIGHTNEIGHBOR1="+sentence(cause.entity.span.last.sentencePos+1).stem
        if (cause.entity.span.first.sentencePos > 1) this += "CAUSELEFTNEIGHBOR2="+sentence(cause.entity.span.first.sentencePos-2).stem
        if (cause.entity.span.last.sentencePos < sentence.length-2) this += "CAUSERIGHTNEIGHBOR2="+sentence(cause.entity.span.last.sentencePos+2).stem
      }
    }

    // span based
    val span = event.clue
    if (stringrepr.matches("[A-Z].*")) this += "STARTSCAP" // capitalization, potentially bad
    if (stringrepr.matches(".*[\\.:;?,].*")) this += "INCLUDESPUNCT" // punctuation, potentially bad
    // better prefix suffix on whole string
    this += "PREFIX2="+stringrepr.substring(0,Math.min(2,stringrepr.length))
    this += "PREFIX3="+stringrepr.substring(0,Math.min(3,stringrepr.length))
    this += "SUFFIX2="+stringrepr.substring(Math.max(0,stringrepr.length-2),stringrepr.length)
    this += "SUFFIX3="+stringrepr.substring(Math.max(0,stringrepr.length-3),stringrepr.length)
    // gazateer
    if (Data.clueGazateer.contains(span.map(_.word).mkString(" ")) || Data.clueGazateer.contains(span.map(_.stem).mkString(" "))) this += "INDICTIONARY"
    span.foreach(this += "SPANPOSINCL="+_.pos) // very important, really
    // type specific gazateers
    for(typInGaz <- Data.cluePerTypeGazateer.keySet) { // makes the result worse!
      if (Data.cluePerTypeGazateer(typInGaz).contains(span.map(_.word).mkString(" ")) || Data.cluePerTypeGazateer(typInGaz).contains(span.map(_.stem).mkString(" ")))
        this += "INDICTIONARY-FOR-"+typInGaz
    }
    if (InitFile.lastWordGenesFeature) {
            this += "LAST-WORD-GENES="+stems+"<--"+
              (if (event.clue.first.sentencePos-1 >= 0) event.clue.sentence.spans.exists(s => s.contains(event.clue.sentence(event.clue.first.sentencePos-1)) && s.isOfGene) else false)
            this += "LAST-WORD-GENES="+stems+"<--"+
              (if (event.clue.first.sentencePos-1 >= 0) event.clue.sentence.spans.exists(s => s.contains(event.clue.sentence(event.clue.first.sentencePos-1)) && s.isOfGene) else false) +":"+
              (if (event.clue.first.sentencePos-2 >= 0) event.clue.sentence.spans.exists(s => s.contains(event.clue.sentence(event.clue.first.sentencePos-2)) && s.isOfGene) else false)
      this += "LAST-WORD-GENES="+stems+"<--"+
              (if (event.clue.first.sentencePos-1 >= 0) event.clue.sentence.spans.exists(s => s.contains(event.clue.sentence(event.clue.first.sentencePos-1)) && s.isOfGene) else false) +":"+
              (if (event.clue.first.sentencePos-2 >= 0) event.clue.sentence.spans.exists(s => s.contains(event.clue.sentence(event.clue.first.sentencePos-2)) && s.isOfGene) else false) +":"+
              (if (event.clue.first.sentencePos-3 >= 0) event.clue.sentence.spans.exists(s => s.contains(event.clue.sentence(event.clue.first.sentencePos-3)) && s.isOfGene) else false)
    }


    //val windowSize = 3
    //for(i <- Math.max(span.start-windowSize,0) to Math.min(span.start+span.length+windowSize,span.sentence.length-1)) {
    //  this += "WINDOW"+windowSize+"="+sentence(i).stem
    //}
    //if (!(sentence.length <= sentencePos+clueLength+1))
    //  this ++= sentence(sentencePos+clueLength+1).values.filter((x:String)=>(!x.contains("@") && !x.contains("WINDOW") && !x.contains("DEP"))).map(x=>"@1"+x) // OC1
    //if (!(sentencePos-1 <= 0))
    //  this ++= sentence(sentencePos-1).values.filter((x:String)=>(!x.contains("@") && !x.contains("WINDOW") && !x.contains("DEP"))).map(x=>"@-1"+x) // OC-1

    if (InitFile.oc) {
      if (sentence.length <= sentencePos+clueLength+1) this += "@1<END>" else {
        this += sentence(sentencePos+clueLength+1).stem+"stem-OC1" // OC1 with stems and words and pos
        this += sentence(sentencePos+clueLength+1).word+"word-OC1"
        this += sentence(sentencePos+clueLength+1).pos+"pos-OC1"
        if (sentence.genes.exists(g=>g.span.contains(sentence(sentencePos+clueLength+1)))) this += "gene-OC1"
        val e = sentence.events.find(e=>e.span.contains(sentence(sentencePos+clueLength+1)))
        if (e.isDefined) this += "event-OC1"+"->"+e.get.eventType.value
      }
      if (sentencePos-1 <= 0) this += "@-1<START>" else {
        this += sentence(sentencePos+clueLength-1).stem+"stem-OC-1" // OC-1 with stems and words and pos
        this += sentence(sentencePos+clueLength-1).word+"word-OC-1"
        this += sentence(sentencePos+clueLength-1).pos+"pos-OC-1"
        if (sentence.genes.exists(g=>g.span.contains(sentence(sentencePos+clueLength-1)))) this += "gene-OC-1"
        val e = sentence.events.find(e=>e.span.contains(sentence(sentencePos+clueLength-1)))
        if (e.isDefined) this += "event-OC-1"+"->"+e.get.eventType.value
      }
    }

    // OC with itself
//    for(f1:String <- this.values ; f2:String <- this.values ; if (f1.hashCode < f2.hashCode)) {
//      this += f1+"@OC@"+f2
//    }


    // debug
//    if (this.values.exists(v => v.contains("MIG"))) {
//      System.err.println(event.clue.sentence.toString(true))
//      for(val a <- this.values) System.err.println(">"+a) ;  System.err.println("---------------------")
//    }
  }

  class EventTemplate extends Template1[Event] with DotStatistics1[EventFeatureVector] { //with SparseWeights {
    val featureVectorCache = new HashMap[String,EventFeatureVector]
    override def freezeDomains = {} // allow to augment the domain
    def statistics(event:Event) = {
      if (event.present) {
        val h = event.hash
        if ((InitFile.featurecaching) && featureVectorCache.contains(h))
            Stat(featureVectorCache(h))
        else {
          val fv = new EventFeatureVector(event)
          if (InitFile.featurecaching) featureVectorCache += h -> fv
          Stat(fv)
        }
      } else Nil
    }
  }


}
