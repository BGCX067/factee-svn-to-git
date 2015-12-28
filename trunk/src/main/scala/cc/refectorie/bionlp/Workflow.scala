package cc.refectorie.bionlp

import cc.refectorie.bionlp.Templates._
import cc.factorie._
import cc.refectorie.bionlp.Sampler.{ArgumentToEventSampler, EventClueAndTypeSampler}
import java.lang.System
import java.io.File
import collection.mutable.{HashSet, ArrayBuffer, HashMap}
import cc.refectorie.bionlp.IntelligentSampler.{MetaSampler, EventsWithThemeOverClues, ClueOnlySampler}

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object Workflow {
  val debug = false
  val neverdebug = false
  var tmpdebug = false
  val rand = new Random

  def showNeverTouched(corpus:Array[Document],iteration:Int) {
    val showNeverTouched = InitFile.initKeyValues("showNeverProposed")=="true"
    val dotouch = InitFile.initKeyValues("showTrueButNeverProposed")
    if (dotouch != null && dotouch == "true") {
      val neverTouchedEvents = for(doc <- corpus ; sentence:Sentence <- doc ; event:Event <- sentence.trueSentence.events ; if (!event.touchedFromProposer)) yield event
      System.err.println("Never proposed in iteration "+iteration+": "+neverTouchedEvents.length)
      val countType = new HashMap[String,Int]
      for(event:Event <- neverTouchedEvents) {
        val et = event.eventType.value
        if (!countType.contains(et)) countType += et -> 1
        else countType += et -> (countType(et)+1)
        if (showNeverTouched) {
          if (!et.contains("egulation")) {
            val varSentence = event.clue.sentence.asInstanceOf[TrueSentence].variableSentence
            val clueExisting = varSentence.spans.exists((s:Span) => s.start==event.clue.start&&s.length==event.clue.length)
            System.err.print(event.outputFormatE)
            System.err.print("\t\t"+(if (clueExisting) "Exist    " else "Not Exist")+" : "+event.outputFormatT)
            event.arguments.foreach(x => {
              val argExisting = varSentence.spans.exists((s:Span) => s.start==x.entity.span.start&&s.length==x.entity.span.length) ;
              System.err.print("\t\t"+(if (argExisting) "Exist    " else "Not Exist")+" : "+(if (x.entity.isInstanceOf[Gene]) x.entity.asInstanceOf[Gene].outputFormat else x.entity.asInstanceOf[Event].outputFormatE))
            })
          }
        }
      }
      for(et <- countType.keySet)
        System.err.println(et+":\t"+countType(et))
    }
  }

  def deberr(s:String) {
    if (debug && !neverdebug) System.err.println(s)
  }


  def inferenceintelligent(documents:Array[Document],model:Model,clueSpanLength:Int, temperature:Double,maxNumOfEventsOnSpan:Int,maxMultipleArgUseAsTheme:Int,clueiterations:Int,jointiterations:Int,eventonlyiterations:Int,putSpansOnAllTokens:Boolean) {
    val metaSam = new MetaSampler(model,null,clueSpanLength,temperature,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme)
    if (!putSpansOnAllTokens) {
      if (clueiterations>0) System.err.println("Inference on clues only")
      for(iteration <- 1 to clueiterations) {
        for (doc <- documents; sentence:Sentence <- doc) {
          for(token <- sentence) { metaSam.process(token) }
        }
        Data.evaluateCorpusOnClues("\t\t\t\t\t\tValidation Evaluation-0 (clue)  ",iteration,documents)
      }
      if (jointiterations>0) System.err.println("Inference jointly")
      for(iteration <- 1 to jointiterations) {
        for (doc <- documents; sentence:Sentence <- doc) {
          for(token <- sentence) { metaSam.process(token) }
          for(span <- sentence.spans; if (span.isOfEvent)) { metaSam.process(span) } // events over clues
        }
        Data.evaluateCorpusOnClues("\t\t\t\t\t\tValidation Evaluation-0 (clue)  ",iteration,documents)
        Data.evaluateCorpusOnEvents("\t\t\t\t\t\tValidation Evaluation-1 (event) ",iteration,documents,false,false,false)
        Data.evaluateCorpusOnEvents("\t\t\t\t\t\tValidation Evaluation-2 (type)  ",iteration,documents,true,false,false)
        Data.evaluateCorpusOnEvents("\t\t\t\t\t\tValidation Evaluation-3 (arg)   ",iteration,documents,true,false,true)
      }
    } else {
      System.err.println("Putting spans on all tokens")
      var count = 0
      for(doc <- documents ; sentence:Sentence <- doc ; token:Token <- sentence ; if (sentence.spansStartingAt(token.sentencePos).isEmpty)) {
        new Span(sentence,token.sentencePos,1,null,"T999")(null)
        count += 1
      }
      System.err.println("There are "+count+" spans added.")
    }

    if (eventonlyiterations>0) System.err.println("Inference on events only")
    for(iteration <- 1 to eventonlyiterations) {
      for (doc <- documents; sentence:Sentence <- doc) {
        for(span <- sentence.spans; if (span.isOfEvent)) { metaSam.process(span) } // events over clues
      }
      // DEBUG XXX
//      documents(0)(0).events.foreach((x:Event) => System.err.println(x.outputFormatE+"\twith clue "+x.clue+"\n\twith args "+x.arguments))
      Data.evaluateCorpusOnClues("\t\t\t\t\t\tValidation Evaluation-0 (clue)  ",iteration,documents)
      Data.evaluateCorpusOnEvents("\t\t\t\t\t\tValidation Evaluation-1 (event) ",iteration,documents,false,false,false)
      Data.evaluateCorpusOnEvents("\t\t\t\t\t\tValidation Evaluation-2 (type)  ",iteration,documents,true,false,false)
      Data.evaluateCorpusOnEvents("\t\t\t\t\t\tValidation Evaluation-3 (arg)   ",iteration,documents,true,false,true)
    }
  }

  def trainIntelligent(model:Model,documents:Array[Document], valDocs:Array[Document], clueSpanLength:Int, temperature:Double, cluesSeparate:Int, joint:Int, eventBasedOnClues:Int,maxNumOfEventsOnSpan:Int,maxMultipleArgUseAsTheme:Int,intermediateiter:Int, clueonlyValidationIterations:Int,jointValidationIterations:Int, eventonlyValidationIterations:Int, writeIntermediateTraining:String,writeIntermediateValidation:String,putSpansOnAllTokensInAdvance:Boolean) {
    val evalTrainIt = InitFile.evaluateOnTrainingAtEachIteration
    //val spanTemplate:SpanTemplate with AllowWeightStats[SpanTemplate] = model.templatesOf[SpanTemplate with AllowWeightStats[SpanTemplate] ].first
    //val spanTemplate = model.templatesOf[SpanTemplate].first
    var obj = new Model
    //obj += new TemplateWithStatistics1[Sentence] { def score(s:Stat) = { val (tp,fp,fn,p,r,f) = s.s1.asInstanceOf[Sentence].evalClue(null,true,1,true) ; tp-0.01*fp} }
    obj += new TemplateWithStatistics1[Span] { def score(s:Stat) = { val r = s.s1.asInstanceOf[Span].isATrueEventSpanInSentence ; if (r) 1 else -1} }
    //obj += new TemplateWithStatistics1[Sentence] { def score(s:Stat) = { val (tp,fp,fn,p,r,f) = s.s1.asInstanceOf[Sentence].eval(true, true, null, false, false, true) ; tp-fp } }
    obj += new TemplateWithStatistics1[Event] { def score(s:Stat) =  { val good=s.s1.asInstanceOf[Event].isATrueEventInSentence(true,false,true,null) ; if (!s.s1.asInstanceOf[Event].present) 0 ; else if (good) {/*System.err.println(1) ;*/ 1} else {/*System.err.println(-1) ;*/ -1} } }

    val metaSam = new MetaSampler(model, obj, clueSpanLength, temperature, maxNumOfEventsOnSpan, maxMultipleArgUseAsTheme) with SampleRank with GradientAscentUpdates with ParameterAveraging

    val allSentences = for(doc <- documents ; sentence:Sentence <- doc ) yield sentence
    val sentencesForClueTraining = allSentences.filter(_.trueSentence.events.length > 0)

    if (!putSpansOnAllTokensInAdvance) {
      System.err.println("Training the clues with "+cluesSeparate+(if (cluesSeparate>1) " iterations." else " iteration."))
      for (iteration <- 1 to cluesSeparate) {
        System.err.println("Iteration "+iteration)
        for(sentence <- sentencesForClueTraining ; token <- sentence) { metaSam.process(token) } // sampling over all tokens which are not in a gene
        if (iteration%evalTrainIt==0)  Data.evaluateCorpusOnClues("Corpus Evaluation-0 (clue)    ",iteration,documents)
        if (valDocs != null && (iteration%intermediateiter==0)) {
          inferenceOnCorpusDuringTraining(model, metaSam, valDocs, clueSpanLength,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,clueonlyValidationIterations,0,0,putSpansOnAllTokensInAdvance)
        }

        if (iteration%intermediateiter==0 && iteration < cluesSeparate) {
          System.err.println("-> Reseting Data Structures on iteration "+iteration+" (every "+intermediateiter+")")
          documents.foreach(_.reset(true))
        }
      }
    } else { //put spans on all tokens
      System.err.println("Putting spans on all tokens")
      var count = 0
      for(sentence:Sentence <- allSentences ; token:Token <- sentence ; if (sentence.spansStartingAt(token.sentencePos).isEmpty)) {
        new Span(sentence,token.sentencePos,1,null,"T999")(null)
        count += 1
      }
      System.err.println("There are "+count+" spans added.")
    }


    if (!putSpansOnAllTokensInAdvance) {
      System.err.println("Training jointly with "+joint+(if (joint>1) " iterations." else " iteration."))
      for (iteration <- 1 to joint) {
        System.err.println("Iteration "+iteration)
        //for(sentence <- sentencesForClueTraining ; token <- sentence) { clueLearner.process(token) } // only the positive ones
        for(sentence <- sentencesForClueTraining ; token <- sentence) { metaSam.process(token) } // all sentences
        for((sentence:Sentence) <- allSentences ; span:Span <- sentence.spans; if (span.isOfEvent)) { metaSam.process(span) }
        if (iteration%evalTrainIt==0)  Data.evaluateCorpusOnClues("Corpus Evaluation-0 (clue) ",iteration,documents)
        if (iteration%evalTrainIt==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-1 (event)",iteration,documents,false,false,false)
        if (iteration%evalTrainIt==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2 (type) ",iteration,documents,true,false,false)
        if (iteration%evalTrainIt==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-3 (arg)  ",iteration,documents,true,false,true)

        if (writeIntermediateTraining!="null") {
          val prefix = writeIntermediateTraining+"/"+iteration+"-joint"
          System.err.println("Writing "+documents.length+" training documents to "+prefix)
          new File(prefix).mkdirs
          documents.foreach((d:Document) => Data.writeDocumentToFiles(d,prefix+"/"+d.id+".txt",prefix+"/"+d.id+".a1",prefix+"/"+d.id+".a2.t1",false))
        }

        if (valDocs != null && (iteration%intermediateiter==0)) {
          inferenceOnCorpusDuringTraining(model, metaSam, valDocs, clueSpanLength,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,clueonlyValidationIterations,jointValidationIterations,eventonlyValidationIterations,putSpansOnAllTokensInAdvance)

          if (writeIntermediateValidation!="null") {
            val prefix = writeIntermediateValidation+"/"+iteration+"-joint"
            System.err.println("Writing "+valDocs.length+" validation documents to "+prefix)
            new File(prefix).mkdirs
            valDocs.foreach((d:Document) => Data.writeDocumentToFiles(d,prefix+"/"+d.id+".txt",prefix+"/"+d.id+".a1",prefix+"/"+d.id+".a2.t1",false))
          }
        }

        // debugging:
        showNeverTouched(documents,iteration)

        if (iteration%intermediateiter==0 && iteration < joint) {
          System.err.println("-> Reseting Data Structures on iteration "+iteration+" (every "+intermediateiter+")")
          documents.foreach(_.reset(true))
        }
      }
    }

    System.err.println("Training events only with "+eventBasedOnClues+(if (eventBasedOnClues>1) " iterations." else " iteration."))
    for (iteration <- 1 to eventBasedOnClues) {
      System.err.println("Iteration "+iteration)
      for((sentence:Sentence) <- allSentences ; span:Span <- sentence.spans; if (span.isOfEvent)) { metaSam.process(span) }
      if (iteration%evalTrainIt==0)  Data.evaluateCorpusOnClues("Corpus Evaluation-0 (clue) ",iteration,documents)
      if (iteration%evalTrainIt==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-1 (event)",iteration,documents,false,false,false)
      if (iteration%evalTrainIt==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2 (type) ",iteration,documents,true,false,false)
      if (iteration%evalTrainIt==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-3 (arg)  ",iteration,documents,true,false,true)
      if (writeIntermediateTraining!="null") {
        val prefix = writeIntermediateTraining+"/"+iteration+"-eventonly"
        System.err.println("Writing "+documents.length+" training documents to "+prefix)
        new File(prefix).mkdirs
        documents.foreach((d:Document) => Data.writeDocumentToFiles(d,prefix+"/"+d.id+".txt",prefix+"/"+d.id+".a1",prefix+"/"+d.id+".a2.t1",false))
      }
      if (valDocs != null && (iteration%intermediateiter==0)) {
        inferenceOnCorpusDuringTraining(model, metaSam, valDocs, clueSpanLength,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,clueonlyValidationIterations,jointValidationIterations,eventonlyValidationIterations,putSpansOnAllTokensInAdvance)

        if (writeIntermediateValidation!="null") {
          val prefix = writeIntermediateValidation+"/"+iteration+"-eventonly"
          System.err.println("Writing "+valDocs.length+" validation documents to "+prefix)
          new File(prefix).mkdirs
          valDocs.foreach((d:Document) => Data.writeDocumentToFiles(d,prefix+"/"+d.id+".txt",prefix+"/"+d.id+".a1",prefix+"/"+d.id+".a2.t1",false))
        }
      }
      // debugging:
      showNeverTouched(documents,iteration)
      if (iteration%intermediateiter==0 && iteration < eventBasedOnClues) {
        System.err.println("-> Reseting Data Structures on iteration "+iteration+" (every "+intermediateiter+")")        
        documents.foreach(_.reset(false))
      }
    }
    metaSam.setWeightsToAverage
  }

  def inferenceOnCorpusDuringTraining(model:Model,sampler:Sampler[_] with ParameterAveraging, valDocs:Array[Document], clueSpanLength:Int,maxNumOfEventsOnSpan:Int,maxMultipleArgUseAsTheme:Int,clueIterations:Int,jointIterations:Int,eventOnlyIterations:Int,putSpansOnAllTokens:Boolean) {
    valDocs.foreach(_.reset(true))
    sampler.setWeightsToAverage
    inferenceintelligent(valDocs,model,clueSpanLength,0.00001,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,clueIterations,jointIterations,eventOnlyIterations,putSpansOnAllTokens)
    sampler.unsetWeightsToAverage
  }

  def inferenceOnCorpus(documents:Array[Document],model:Model,iterClueType:Int,iterArg:Int,iterAll:Int,clueSpanLength:Int, allowEventOnSameSpanStart:Boolean,temperature:Double) {
    val eventClueInf        = new EventClueAndTypeSampler(model,null,true,clueSpanLength,allowEventOnSameSpanStart,temperature)
    val argumentsToEventInf = new ArgumentToEventSampler(model,null,temperature)
    val testTokens = for (doc <- documents; sentence <- doc; token <- sentence) yield token
    val testSentences = for (doc <- documents; sentence <- doc) yield sentence
    val testEvents = for (doc <- documents; sentence <- doc; event <- sentence.events) yield event
    if (iterClueType > 1) System.err.println("Testing with "+iterClueType+" iterations on clue and event type:")
    for (iteration <- 1 to iterClueType) {
      for (token <- testTokens) { eventClueInf.process(token) }
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-1",iteration,documents,false,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2",iteration,documents,true,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-3",iteration,documents,true,false,true)
    }
    if (iterArg > 1) System.err.println("Testing with "+iterClueType+" iterations on arguments:")
    for (iteration <- 1 to iterArg) {
      for(event:Event <- testEvents) { argumentsToEventInf.process(event) }
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-1",iteration,documents,false,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2",iteration,documents,true,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-3",iteration,documents,true,false,true)
    }
    if (iterAll > 1 ) System.err.println("Testing with "+iterAll+" iterations on all:")
    for (iteration <- 1 to iterAll) {
      for (token <- testTokens) { eventClueInf.process(token) }
      for(event:Event <- testEvents) { argumentsToEventInf.process(event) }
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-1",iteration,documents,false,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2",iteration,documents,true,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-3",iteration,documents,true,false,true)
    }
    ;
  }

	def main(args:Array[String]) : Unit = {
    System.err.println("Starting training and testing workflow...")
    System.err.println("Parameters specified: "+args.length)
    args.foreach(x => System.err.println(x))
    System.err.println
    if (args.length != 1 || !(new File(args(0)).exists)) {
      System.err.println("You need to specify an .ini file (as in the init folder).")
      System.exit(1)
    }


    InitFile.initialize(args(0))

    Domain[SpanFeatureVector].maxSize = 1000000
    Domain[EventFeatureVector].maxSize = 1000000
    Domain[ArgumentPairFeatureVector].maxSize = 1000000
    Domain[EventClueArgumentFeatureVector].maxSize = 1000000



    val useAll = InitFile.initKeyValues("useAll").toBoolean
    val clueSpanLength = InitFile.initKeyValues("clueSpanLength").toInt
    val allowEventOnSameSpanStart = InitFile.initKeyValues("allowEventOnSameSpanStart").toBoolean // DOES NOT WORK
    val temperature = InitFile.initKeyValues("temperature").toDouble
    val clueTraining = InitFile.initKeyValues("clueTraining").toInt
    val jointTraining = InitFile.initKeyValues("jointTraining").toInt
    val eventAloneTraining = InitFile.initKeyValues("eventAloneTraining").toInt
    val validation = InitFile.initKeyValues("validate").toBoolean
    val maxNumOfEventsOnSpan = InitFile.initKeyValues("maxNumOfEventsOnSpan").toInt
    val maxMultipleArgUseAsTheme = InitFile.initKeyValues("maxMultipleArgUseAsTheme").toInt
    val validationResetEachNumOfIteration = InitFile.initKeyValues("validationResetEachNumOfIteration").toInt
    val clueValidationIterations = InitFile.initKeyValues("clueValidation").toInt
    val jointValidationIterations = InitFile.initKeyValues("jointValidation").toInt
    val eventOnlyValidationIterations = InitFile.initKeyValues("eventAloneValidation").toInt
    val folds = if (InitFile.initKeyValues.contains("folds")) InitFile.initKeyValues("folds").toInt else 8
    val putSpansOnAllTokens = if (InitFile.initKeyValues.contains("putSpansOnAllTokens")) InitFile.initKeyValues("putSpansOnAllTokens").toBoolean else false  // DOES NOT WORK

    // the model
    val model = new Model
    if (!putSpansOnAllTokens) model += (new SpanTemplate).init
	  model += (new EventTemplate).init
    model += (new EventClueArgumentTemplate).init
    model += (new ArgumentPairTemplate).init

    var trainDocs1 = Data.readCorpus(InitFile.initKeyValues("trainfile"))
    var valDocs1 = if (validation && useAll) Data.readCorpus(InitFile.initKeyValues("valfile")) else null

    Data.checkEntityHomogenityOnSpans = true
    
    // separating training and validation set
    val (trainDocs,valDocs) = if (useAll)
      (trainDocs1,valDocs1)
    else Data.combineSubSampleToTrainingAndValidation(Data.cvSets(trainDocs1,folds),0)

    // extract gazateer
    for(doc <- trainDocs ; s <- doc ; span:Span <- s.trueSentence.spans ; if (span.isOfEvent)) Data.clueGazateer.addEntry(span.map(_.word).mkString(" "))
    for(doc <- trainDocs ; s <- doc ; span:Span <- s.trueSentence.spans ; if (span.isOfEvent)) Data.clueGazateer.addEntry(span.map(_.stem).mkString(" "))
    System.err.println("Gazateer: "+Data.clueGazateer.size+" entries.")
    for(doc <- trainDocs ; s <- doc ; span:Span <- s.trueSentence.spans ; if (span.isOfEvent)) for(token <- span) Data.clueTokenGazateer.addEntry(token.stem)


    System.err.println("Train/Validation Set Size: "+trainDocs.size+"/"+(if (valDocs==null) 0 else valDocs.size))

    //trainAlltogether(model,trainDocs,jointTraining,clueSpanLength,true,validationResetEachNumOfIteration)

    trainIntelligent(
      model,
      trainDocs,
      if (validation) valDocs else null,
      clueSpanLength,
      temperature,
      clueTraining,
      jointTraining,
      eventAloneTraining,
      maxNumOfEventsOnSpan,
      maxMultipleArgUseAsTheme,
      validationResetEachNumOfIteration,
      clueValidationIterations,
      jointValidationIterations,
      eventOnlyValidationIterations,
      InitFile.initKeyValues("writeIntermediateTraining"),
      InitFile.initKeyValues("writeIntermediateValidation"),
      putSpansOnAllTokens
      )

    val writePrefixTraining = InitFile.initKeyValues("writeFinalTraining")
    if (writePrefixTraining != "null")
      trainDocs.foreach((d:Document) => Data.writeDocumentToFiles(d,writePrefixTraining+"/"+d.id+".txt",writePrefixTraining+"/"+d.id+".a1",writePrefixTraining+"/"+d.id+".a2.t1",false))
    val writePrefixVal = InitFile.initKeyValues("writeFinalValidation")
    if (valDocs != null && writePrefixVal != "null")
      valDocs.foreach((d:Document) => Data.writeDocumentToFiles(d,writePrefixVal+"/"+d.id+".txt",writePrefixVal+"/"+d.id+".a1",writePrefixVal+"/"+d.id+".a2.t1",false))

    Data.showAllFPEvents(valDocs,false,false,false,true)
	}


}
