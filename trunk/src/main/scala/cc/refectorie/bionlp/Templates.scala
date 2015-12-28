package cc.refectorie.bionlp

import cc.factorie._
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object Templates {

  class EventClueArgumentFeatureVector(event:Event,arg:Argument) extends BinaryVectorVariable[String] {
    // if (event == null && arg == null) this // stupid
    for(t1 <- event.clue.spannedTokens) {
      for(t2 <- arg.entity.span.spannedTokens) {
        //if (StanfordDependencies.tokenToDep.contains(t1) && StanfordDependencies.tokenToDep(t1).contains(t2)) {
        //this += "SD="+StanfordDependencies.tokenToDep(t1)(t2).dependencyType
        // }
        val path:Iterable[String] = if (InitFile.initKeyValues.contains("selftokenized") && InitFile.initKeyValues("selftokenized")=="true")
          StanfordDependencies.featureFromPath(t1,t2,InitFile.pathngrams,null,false,"")
        else if (InitFile.initKeyValues.contains("mc") && InitFile.initKeyValues("mc")=="true")
          StanfordDependencies.featureFromPath(t1,t2,InitFile.pathngrams,null,true,"")
        else Nil
//        path.foreach(x => {this += x})                                      // not good for the tiny example! XXX
        path.foreach(x => {this += x+"_WITH_TYPE_"+event.eventType.value})  // not good for the tiny example! XXX
        path.foreach(x => {this += x+"_WITH_ROLE_"+arg.roleType.value})     // not good for the tiny example! XXX
        path.foreach(x => {this += x+"_WITH_TYPE_"+event.eventType.value+"_WITH_ROLE_"+arg.roleType.value+"_"+event.clue.stringrepr})
//        path.foreach(x => {this += x+"_WITH_TYPE_"+event.eventType.value+"_WITH_ROLE_"+arg.roleType.value+"_"+event.clue.stringrepr+"_"+arg.entity.span.stringrepr})
        path.foreach(x => {this += x+"_WITH_TYPE_"+event.eventType.value+"_WITH_ROLE_"+arg.roleType.value})
        ;
      }
    }
    // this whole block is not good for the tiny example! XXX
    this += "STEMS="+event.clue.map(_.stem).mkString(" ")+"---"+arg.entity.span.map(_.stem).mkString(" ")
    this += "STRINGS="+event.clue.stringrepr+"---"+arg.entity.span.stringrepr
    this += "STRINGS="+event.clue.stringrepr+"---"+arg.entity.span.stringrepr+"_WITH_ROLE"+arg.roleType.value
    this += "STRINGS="+event.clue.stringrepr+"---"+arg.entity.span.stringrepr+"_WITH_TYPE"+event.eventType.value
    this += "STRINGS="+event.clue.stringrepr+"---"+arg.entity.span.stringrepr+"_WITH_TYPE"+event.eventType.value+"_WITH_ROLE"+arg.roleType.value
    this += "EVENTCLUESTEM="+event.clue.map(_.stem).mkString(" ")
    this += "EVENTCLUESTRING="+event.clue.stringrepr
    this += "ARGSTEM="+arg.entity.span.map(_.stem).mkString(" ")
    this += "ARGSTRING="+arg.entity.span.stringrepr
    this += "ARGTYPE="+arg.roleType.value
    this += "ARGTYPEANDSTRING="+arg.roleType.value+"---"+arg.entity.span.stringrepr
    if (arg.entity.isInstanceOf[Gene]) this += "ARGISGENE"
    if (arg.entity.isInstanceOf[Event]) this += "ARGISEVENT"
    arg.entity.span.foreach(x => this += "ARGPOS="+x.pos)
    event.clue.foreach(x => this += "EVENTPOS="+x.pos)
    for(a <-arg.entity.span ; e <- event.clue) this += "ARGEVENTPOS="+a.pos+"-"+e.pos
    for(a <-arg.entity.span ; e <- event.clue) this += "ARGEVENTPOS-TYPE="+a.pos+"-"+e.pos+event.eventType.value
    if (!event.eventType.value.equals("NOTYPE")) this += "EVENTTYPE="+event.eventType.value
    // this whole block is not good for the tiny example! XXX

//        this += event.id+":"+arg.id

    // DEBUG XXX
    if (InitFile.debugFeaturesForTiny && event.clue.first.word=="inhibit" && event.arguments.exists(a => a.entity.span.first.word=="activation" && a.roleType.value=="Theme") && event.arguments.exists(a => a.entity.span.first.word=="inducing" && a.roleType.value=="Cause") ) {
      System.err.println("In EventClueArgumentFeatureVector: "+event+" arg: "+event.arguments+" ("+arg+")")
      for(val a <- this.values) System.err.println(a)
      System.err.println()
    }
//    System.err.println(event.clue.sentence.toString(true))
//    for(val a <- this.values) System.err.println(">"+a) ;  System.err.println("---------------------")

    def ++=(other:EventClueArgumentFeatureVector) = {other.values.foreach(x => this += x)}
  }

  class EventClueArgumentTemplate extends Template2[Event,Argument] with DotStatistics1[EventClueArgumentFeatureVector] with AllowWeightStats[EventClueArgumentFeatureVector] {
    val featureVectorCache = new HashMap[String,EventClueArgumentFeatureVector]
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(event:Event) = {
      if (event.present)
        for(arg <- event.arguments) yield Factor(event,arg)
      else Nil
    }
    def unroll2(arg:Argument) = {
      if (arg.present)
        Factor(arg.eventWithThisArgument,arg)
      else
        Nil
    }
    def statistics(event:Event, arg:Argument):Stat = {
      val h = event.hash+"->"+arg.hash
      if ((InitFile.featurecaching) && featureVectorCache.contains(h))
        Stat(featureVectorCache(h))
      else {
        val fv = new EventClueArgumentFeatureVector(event,arg)
        if (InitFile.featurecaching) featureVectorCache += h -> fv
        Stat(fv)
      }
    }
  }

  class ArgumentPairFeatureVector(a1:Argument,a2:Argument) extends BinaryVectorVariable[String] {
    val onlySentenceAsFeature = false
    if (a1!=null && a2!=null) {
      if (!onlySentenceAsFeature) {
        for(t1 <- a1.entity.span.spannedTokens) {
          for(t2 <- a2.entity.span.spannedTokens) {
            //System.err.println(">>>"+StanfordDependencies.tokenToDep)
            if (StanfordDependencies.tokenToDepForward.contains(t1) && StanfordDependencies.tokenToDepForward(t1).contains(t2)) {
              //System.err.println("Found    \t"+t1+"\t"+t2)
              this += "SD="+StanfordDependencies.tokenToDepForward(t1)(t2).dependencyType
            }
          }
        }
        this += "STRINGS="+a1.entity.span.map(_.word).mkString(" ")+"---"+a2.entity.span.map(_.word).mkString(" ")
        this += "A1STRING="+a1.entity.span.map(_.word).mkString(" ")
        this += "A2STRING="+a2.entity.span.map(_.word).mkString(" ")
        if (a1.entity.isInstanceOf[Gene]) this += "ISGENE=A1"
        if (a2.entity.isInstanceOf[Gene]) this += "ISGENE=A2"
        if (a1.entity.isInstanceOf[Event]) this += "ISEVENT=A1"
        if (a2.entity.isInstanceOf[Event]) this += "ISEVENT=A2"
        this += "ARG1TYPE="+a1.roleType.value
        this += "ARG2TYPE="+a1.roleType.value
        this += "ARG1TYPEANDSTRING="+a1.roleType.value+"---"+a1.entity.span.map(_.word).mkString(" ")
        this += "ARG2TYPEANDSTRING="+a1.roleType.value+"---"+a2.entity.span.map(_.word).mkString(" ")
      }
      if (onlySentenceAsFeature) this += "SENTENCE="+a1.entity.span.sentence.map(_.word).mkString(" ")
    }
    //System.err.println("ArgumentFeatures: "+this)
    def ++=(other:ArgumentPairFeatureVector) = {other.values.foreach(x => this += x)}
  }

  class ArgumentPairTemplate extends Template2[Argument,Argument] with DotStatistics1[ArgumentPairFeatureVector] with AllowWeightStats[ArgumentPairFeatureVector] {
    override def freezeDomains = {} // allow to augment the domain
    def unroll1(arg:Argument) = {
      for((other:Argument) <- arg.eventWithThisArgument.arguments ; if (other.entity.span.start < arg.entity.span.start)) yield Factor(arg,other)
    }
    def unroll2(arg:Argument) = {
      for((other:Argument) <- arg.eventWithThisArgument.arguments ; if (other.entity.span.start > arg.entity.span.start)) yield Factor(arg,other)
    }

    def statistics(a1:Argument, a2:Argument):Stat = {
      Stat(new ArgumentPairFeatureVector(a1,a2))
    }
  }

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

  class EventFeatureVector(event:Event) extends BinaryVectorVariable[String] {
    val stringrepr = event.clue.map(_.word).mkString(" ")
    val sentence = event.clue.sentence
    val sentencePos = event.clue.start
    val clueLength = event.clue.length
    this += "DEFAULT"
    if (!event.eventType.value.equals("NOTYPE")) this += "TYPE="+event.eventType.value
    if (!event.eventType.value.equals("NOTYPE")) this += "WORD-AND-TYPE="+event.eventType.value + "->" + stringrepr
    this += "STRING="+stringrepr
    
    //event.clue.foreach(this += "WORDINCL="+_.word)
    //event.clue.foreach(this += "STEMINCL="+_.stem)
    //if (sentence.length <= sentencePos+clueLength+1) this += "@1<END>" else this ++= sentence(sentencePos+clueLength+1).values.filter((x:String)=>(!x.contains("@"))).map(x=>"@1"+x) // OC1
    //if (sentencePos-1 <= 0) this += "@-1<START>" else this ++= sentence(sentencePos-1).values.filter((x:String)=>(!x.contains("@"))).map(x=>"@-1"+x) // OC-1
    // this += "NUMOFGENESINSENTENCE="+event.clue.sentence.genes.length

    //for(v <- new SpanFeatureVector(event.clue).values) this += v // while not using the clue sampler!

    // DEBUG XXX
    if (InitFile.debugFeaturesForTiny && event.clue.first.word=="inhibit" && event.arguments.exists(a => a.entity.span.first.word=="activation" && a.roleType.value=="Theme") && event.arguments.exists(a => a.entity.span.first.word=="inducing" && a.roleType.value=="Cause") ) {
      System.err.println("In EventFeatureVector: "+event+" arg: "+event.arguments)
      for(val a <- this.values) System.err.println(a)
      System.err.println()
    }
    // debug
//    System.err.println(event.clue.sentence.toString(true))
//    for(val a <- this.values) System.err.println(">"+a) ;  System.err.println("---------------------")
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

  class SpanFeatureVector(span:Span) extends BinaryVectorVariable[String] {
    val onlySentenceAndSpan = false
    if (!onlySentenceAndSpan) {
      val stringrepr:String = span.stringrepr
      val stringflat:String = span.map(_.word).mkString(" ")
      val sentence = span.sentence
      val sentencePos = span.start
      val clueLength = span.length
      this += "SPANDEFAULT"
      this += "SPANSTRING="+stringrepr     // full string representation
      this += "SPANSTEM="+span.map(_.stem).mkString(" ") // stem, potentially bad
      span.foreach(this += "SPANSTEMINCL="+_.stem) // stem, potentially bad
      if (stringrepr.matches("[A-Z].*")) this += "STARTSCAP" // capitalization, potentially bad
      if (stringrepr.matches(".*[\\.:;?,].*")) this += "INCLUDESPUNCT" // punctuation, potentially bad
      //for(i <- 0 until stringflat.length-2) this += "TRIGRAM="+stringflat(i)+stringflat(i+1)+stringflat(i+2) // trigrams SLOW, until now, really bad: overfitting
      //for(i <- 0 until stringflat.length-1) this += "BIGRAM="+stringflat(i)+stringflat(i+1) // bigrams SLOW, until now, really bad: overfitting
//      span.map(_.word).foreach(x => this += "PREFIX2="+x.substring(0,Math.min(2,x.length))) //  potentially bad
//      span.map(_.word).foreach(x => this += "PREFIX3="+x.substring(0,Math.min(3,x.length))) //  potentially bad
//      span.map(_.word).foreach(x => this += "SUFFIX2="+x.substring(Math.max(0,x.length-2),x.length)) //  potentially bad
//      span.map(_.word).foreach(x => this += "SUFFIX3="+x.substring(Math.max(0,x.length-3),x.length)) //  potentially bad
      // better prefix suffix on whole string
      this += "PREFIX2="+stringrepr.substring(0,Math.min(2,stringrepr.length))
      this += "PREFIX3="+stringrepr.substring(0,Math.min(3,stringrepr.length))
      this += "SUFFIX2="+stringrepr.substring(Math.max(0,stringrepr.length-2),stringrepr.length)
      this += "SUFFIX3="+stringrepr.substring(Math.max(0,stringrepr.length-3),stringrepr.length)
      // gazateer
      if (Data.clueGazateer.contains(span.map(_.word).mkString(" ")) || Data.clueGazateer.contains(span.map(_.stem).mkString(" "))) this += "INDICTIONARY"
      span.foreach(this += "SPANWORDINCL="+_.word) // very important, perhaps
      span.foreach(this += "SPANPOSINCL="+_.pos) // very important, really

      // get dependency paths of max length 3 // TODO implement for mc
      val ngrams = new Array[Int](2) ; ngrams(0) = 1 ; ngrams(1) = 2
      //System.err.println("TESET "+span.sentence.dependencyPaths(span(0)).keySet.size)
      if (InitFile.selftokenizedpath) {
        for((t:Token) <- span) {
          val pathsUntilLength3 = new HashMap[Token,ArrayBuffer[(SDependency,Boolean)]]
          val pathsForToken = span.sentence.dependencyPaths(t)
          for(otherToken:Token <- pathsForToken.keySet) {
            if (pathsForToken != null && pathsForToken.contains(otherToken) && pathsForToken(otherToken) != null && pathsForToken(otherToken).length < 4) {
              //System.err.println("From "+t+" to "+otherToken)
              val paths = StanfordDependencies.featureFromPath(t,otherToken,ngrams,null,false,"CLUE-")
              //paths.foreach(x => System.err.println("PATH: "+x))
              // add featuresWithStats of dependent token, not nice code, sorry
              this += "DEPSTRING="+otherToken.word
              this += "DEPSTEM="+otherToken.stem
              if (otherToken.word.matches("[A-Z].*")) this += "DEPSTARTSCAP"
              if (otherToken.word.matches(".*[\\.:;?,].*")) this += "DEPINCLUDESPUNCT"
              this += "DEPPOS="+otherToken.pos
              if (Data.clueTokenGazateer.contains(otherToken.stem)) this += "DEPINDICTIONARY"
              paths.foreach(x => this += x) // add path featuresWithStats themselves
            }
          }
        }
      }
      if (InitFile.mcpath) {
        for((t:Token) <- span) {
          val pathsUntilLength3 = new HashMap[Token,ArrayBuffer[(SDependency,Boolean)]]
          val pathsForToken = span.sentence.dependencyPathsMc(t)
          for(otherToken:Token <- pathsForToken.keySet) {
            if (pathsForToken != null && pathsForToken.contains(otherToken) && pathsForToken(otherToken) != null && pathsForToken(otherToken).length < 4) {
              //System.err.println("From "+t+" to "+otherToken)
              val paths = StanfordDependencies.featureFromPath(t,otherToken,ngrams,null,true,"CLUE-")
              //paths.foreach(x => System.err.println("PATH: "+x))
              // add featuresWithStats of dependent token, not nice code, sorry
              this += "DEPSTRING="+otherToken.word
              this += "DEPSTEM="+otherToken.stem
              if (otherToken.word.matches("[A-Z].*")) this += "DEPSTARTSCAP"
              if (otherToken.word.matches(".*[\\.:;?,].*")) this += "DEPINCLUDESPUNCT"
              this += "DEPPOS="+otherToken.pos
              if (Data.clueTokenGazateer.contains(otherToken.stem)) this += "DEPINDICTIONARY"
              paths.foreach(x => this += x) // add path featuresWithStats themselves
            }
          }
        }
      }

      this += "NUMOFGENESINSENTENCE"+(span.sentence.genes.length)
      // featuresWithStats in window
      //      val windowSize = 3
      //      for(i <- Math.max(span.start-windowSize,0) to Math.min(span.start+span.length+windowSize,span.sentence.length-1)) {
      //        this += "WINDOW"+windowSize+"="+sentence(i).stem
      //      }
      //      if (!(sentence.length <= sentencePos+clueLength+1))
      //        this ++= sentence(sentencePos+clueLength+1).values.filter((x:String)=>(!x.contains("@") && !x.contains("WINDOW") && !x.contains("DEP"))).map(x=>"@1"+x) // OC1
      //      if (!(sentencePos-1 <= 0))
      //        this ++= sentence(sentencePos-1).values.filter((x:String)=>(!x.contains("@") && !x.contains("WINDOW") && !x.contains("DEP"))).map(x=>"@-1"+x) // OC-1

      ;
    } else {
      this += "SENTENCE="+span.sentence.map(_.word).mkString(" ")+"---"+span.map(_.word).mkString(" ")+span

    }
    //println(this)
  }
  class SpanTemplate extends Template1[Span] with DotStatistics1[SpanFeatureVector] with AllowWeightStats[SpanFeatureVector] {
    val featureVectorCache = new HashMap[String,SpanFeatureVector]
    override def freezeDomains = {} // allow to augment the domain
    def statistics(span:Span) = {
      if (span.sentence.spans.contains(span)) {
        val h = span.hash
        if ((InitFile.featurecaching) && featureVectorCache.contains(h))
          Stat(featureVectorCache(h))
        else {
          val fv = new SpanFeatureVector(span)
          if (InitFile.featurecaching) featureVectorCache += h -> fv
          Stat(fv)
        }
      } else {
        Nil
      }
    }
  }

  class TokenFeatureVector(token:Token) extends BinaryVectorVariable[String] {
    token.values.foreach(this += _)
    //System.err.println("TokenFeaturevector: "+this)
  }
  class TokenTemplate extends Template1[Token] with DotStatistics1[TokenFeatureVector] {
    override def freezeDomains = {} // allow to augment the domain
    def statistics(token:Token) = {
      Stat(new TokenFeatureVector(token))
    }
  }
}



/**
 * A possible move for the proposer to choose
 */
case class Choice(probability: Double, change: DiffList => Unit) {
  /**
   * For representing the difflist, model and true score of a choice
   */
  case class ScoredDiffList(model: Model, objective: Model) {
    val difflist = new DiffList
    change(difflist)
    val (modelScore, trueScore) = difflist.scoreAndUndo(model, objective)

    override def toString = modelScore + "," + trueScore + difflist.mkString("{", ",", "}")
  }

  /**
   * Return the difflist plus model score/objective score for this choice
   */
  def scoredDiffList(model: Model, objective: Model) = ScoredDiffList(model, objective)

  def asProposal(model: Model, objective: Model): Proposal = {
    val sd = ScoredDiffList(model, objective)
    Proposal(sd.difflist, sd.modelScore, sd.trueScore, probability)
  }

  def asProposalWithScoreAcceptance(model: Model, objective: Model, temperature: Double): Proposal = {
    val sd = ScoredDiffList(model, objective)
    //     Workflow.deberr(Workflow.tmpdebug,
    //       "Setting values: "+"ms: "+sd.modelScore+"; os: "+sd.trueScore+"; as: "+(sd.modelScore / temperature)+"; temp: "+temperature)
    //Parameters: diff:DiffList, modelScore:Double, objectiveScore:Double,  acceptanceScore:Double
    if (sd.modelScore != 0.0) if (Workflow.debug) Workflow.deberr("ModScore 2: "+sd.modelScore)
    Proposal(sd.difflist, sd.modelScore, sd.trueScore, sd.modelScore / temperature)
  }
}