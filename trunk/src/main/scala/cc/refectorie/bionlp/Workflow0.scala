package cc.refectorie.bionlp

import collection.mutable.{HashSet, HashMap}
import cc.refectorie.bionlp.Sampler.{ArgumentToEventSampler, EventClueAndTypeSampler}
import java.io.File
import cc.factorie._
import cc.refectorie.bionlp.Templates._

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object Workflow0 {
  val debug = false
  val neverdebug = false
  var tmpdebug = false
  //val rand = new Random

  def trainClues(model:Model,documents:Array[Document],valDocs:Array[Document],iterations:Int,includeType:Boolean,clueSpanLength:Int,allowEventOnSameSpanStart:Boolean,maxNumOfEventsOnSpan:Int,maxMultipleArgUseAsTheme:Int,temperature:Double,clueValidationIterations:Int,jointValidationIterations:Int,eventOnlyValidationIterations:Int,putSpansOnAllTokens:Boolean) {
    System.err.println("Training the clues with "+iterations+" iterations.")
    val trainTokens = for (doc <- documents; sentence <- doc; token <- sentence) yield token
    val cluesObj = new Model
    cluesObj += new TemplateWithStatistics1[Sentence] { def score(s:Stat) = { s.s1.asInstanceOf[Sentence].eval(includeType,false,false,true)._6 } }
    //cluesObj += new TemplateWithStatistics1[Sentence] { def score(s:Stat) = { s.s1.asInstanceOf[Sentence].acc(includeType,false) } }
    //if (includeType) cluesObj += new TemplateWithStatistics1[Event] { def score(s:Stat) = { if (s.s1.asInstanceOf[Event].isATrueEventInSentence(true,false)) 1.0 else 0.0 } }
    val eventClueLearner = new EventClueAndTypeSampler(model,cluesObj,includeType, clueSpanLength, allowEventOnSameSpanStart,temperature) with SampleRank with GradientAscentUpdates //with ParameterAveraging
    for (iteration <- 1 to iterations) {
      for (token <- trainTokens) {
        eventClueLearner.process(token) // uses an objective which does not check the eventtype
      }

      if (valDocs != null && (iteration%1==0)) {
        inferenceOnCorpusDuringTraining(model,null,valDocs,clueSpanLength,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,clueValidationIterations,jointValidationIterations,eventOnlyValidationIterations,putSpansOnAllTokens)
      }


      if (iteration%1==0)                Data.evaluateCorpusOnEvents("Corpus Evaluation-1",iteration,documents,false,false,false)
      if (includeType && iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2",iteration,documents,true,false,false)
    }
  }

  def trainArgumentsOnEvents(model:Model,documents:Array[Document],valDocs:Array[Document],clueSpanLength:Int,iterations:Int,maxNumOfEventsOnSpan:Int,maxMultipleArgUseAsTheme:Int,temperature:Double,clueValidationIterations:Int,jointValidationIterations:Int,eventOnlyValidationIterations:Int,putSpansOnAllTokens:Boolean) {
    System.err.println("Training the arguments with "+iterations+" iterations.")
    val argumentAtEventObj = new Model
    argumentAtEventObj += new TemplateWithStatistics1[Event] { def score(s:Stat) = { s.s1.asInstanceOf[Event].evalFMeasureOfArguments(null)._6 } }
    val argumentsToEventLearner = new ArgumentToEventSampler(model,argumentAtEventObj,temperature) with SampleRank with GradientAscentUpdates with ParameterAveraging
    //argumentsToEventLearner.learningRate = 0.1
    argumentsToEventLearner.logLevel = 0 //-2
    val trainEvents = for (doc <- documents; sentence <- doc; event <- sentence.events) yield event
    for (iteration <- 1 to iterations) {
      for(event:Event <- trainEvents) {
        //System.err.println("Arguments in event "+event.clue.spannedTokens.mkString(" ")+": "+event.arguments.map(_.entity.span.spannedTokens.map(_.word).mkString(" ")))
        argumentsToEventLearner.process(event) // uses an objective which does not check the eventtype
        //System.err.println("Arguments in event "+event.clue.spannedTokens.mkString(" ")+": "+event.arguments.map(_.entity.span.spannedTokens.map(_.word).mkString(" ")))
        //val truNum = event.arguments.filter((x:Argument)=> x.isATrueArgumentInEvent(event,true)).length
        //Workflow.deberr(true,"EventArg TP,FP,FN\t"+truNum+"\t"+(event.arguments.length-truNum)+"\t"+event.numberOfMissedArguments+"\t"+event.clue.spannedTokens.map(_.word).mkString(" "))
        ;
      }

      if (valDocs != null && (iteration%1==0)) {
        inferenceOnCorpusDuringTraining(model,argumentsToEventLearner,valDocs,clueSpanLength,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,clueValidationIterations,jointValidationIterations,eventOnlyValidationIterations,putSpansOnAllTokens)
      }
      
      //if (iteration%10==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-1",iteration,documents,false,false)
      //if (iteration%10==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2",iteration,documents,true,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-1",iteration,documents,false,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2",iteration,documents,true,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-3",iteration,documents,true,false,true)
    }
  }

  def trainAlltogether(model:Model,documents:Array[Document],valDocs:Array[Document],iterations:Int,clueSpanLength:Int, allowEventOnSameSpanStart:Boolean, maxNumOfEventsOnSpan:Int, maxMultipleArgUseAsTheme:Int, clueonlyValidationIterations:Int, jointValidationIterations:Int, eventonlyValidationIterations:Int, temperature:Double, intermediateiter:Int) {
    System.err.println("Training all together with "+iterations+" iterations.")
    val cluesObj = new Model ; cluesObj += new TemplateWithStatistics1[Sentence] { def score(s:Stat) = { s.s1.asInstanceOf[Sentence].eval(true,false,false,true)._6 } }
    val eventClueLearner = new EventClueAndTypeSampler(model,cluesObj,true,clueSpanLength, allowEventOnSameSpanStart, temperature) with SampleRank with GradientAscentUpdates with ParameterAveraging
    val argumentAtEventObj = new Model ; argumentAtEventObj += new TemplateWithStatistics1[Event] { def score(s:Stat) = { s.s1.asInstanceOf[Event].evalFMeasureOfArguments(null)._6 } }
    val argumentsToEventLearner = new ArgumentToEventSampler(model,argumentAtEventObj,temperature) with SampleRank with GradientAscentUpdates with ParameterAveraging

    for (iteration <- 1 to iterations) {
      for(doc <- documents; sentence <- doc; token <- sentence) { eventClueLearner.process(token) }
      for(doc <- documents; sentence <- doc; event <- sentence.events) { argumentsToEventLearner.process(event) }
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-1",iteration,documents,false,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-2",iteration,documents,true,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("Corpus Evaluation-3",iteration,documents,true,false,true)

      if (valDocs != null && (iteration%intermediateiter==0)) {
          inferenceOnCorpusDuringTraining(model, null, valDocs, clueSpanLength,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,clueonlyValidationIterations,jointValidationIterations,eventonlyValidationIterations,false)
      }

      if (iteration%intermediateiter==0 && iteration < iterations) {
        System.err.println("-> Reseting Data Structures on iteration "+iteration+" (every "+intermediateiter+")")
        documents.foreach(_.reset(true))
      }
    }
  }



  def inferenceOnCorpusDuringTraining(model:Model,sampler:Sampler[_] with ParameterAveraging, valDocs:Array[Document], clueSpanLength:Int,maxNumOfEventsOnSpan:Int,maxMultipleArgUseAsTheme:Int,clueIterations:Int,jointIterations:Int,eventOnlyIterations:Int,putSpansOnAllTokens:Boolean) {
    valDocs.foreach(_.reset(true))
    sampler.setWeightsToAverage
    inferenceOnCorpus(valDocs,model,clueIterations,eventOnlyIterations,jointIterations,clueSpanLength, true,0.00001)
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
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-1",iteration,documents,false,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-2",iteration,documents,true,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-3",iteration,documents,true,false,true)
    }
    if (iterArg > 1) System.err.println("Testing with "+iterClueType+" iterations on arguments:")
    for (iteration <- 1 to iterArg) {
      for(event:Event <- testEvents) { argumentsToEventInf.process(event) }
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-1",iteration,documents,false,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-2",iteration,documents,true,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-3",iteration,documents,true,false,true)
    }
    if (iterAll > 1 ) System.err.println("Testing with "+iterAll+" iterations on all:")
    for (iteration <- 1 to iterAll) {
      for (token <- testTokens) { eventClueInf.process(token) }
      for(event:Event <- testEvents) { argumentsToEventInf.process(event) }
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-1",iteration,documents,false,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-2",iteration,documents,true,false,false)
      if (iteration%1==0) Data.evaluateCorpusOnEvents("\t\t\tCorpus Evaluation-3",iteration,documents,true,false,true)
    }
    ;
  }

	def main(args:Array[String]) : Unit = {
    System.err.println("Starting training and testing workflow 0 (the old one!)...")
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
    //model += (new ArgumentPairTemplate).init

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


    trainClues(model,trainDocs,valDocs,clueTraining,true,clueSpanLength,allowEventOnSameSpanStart,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,temperature,clueValidationIterations,jointValidationIterations,eventOnlyValidationIterations,putSpansOnAllTokens)

    trainArgumentsOnEvents(model,trainDocs,valDocs,clueSpanLength,eventAloneTraining,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,temperature,clueValidationIterations,jointValidationIterations,eventOnlyValidationIterations,putSpansOnAllTokens)

    trainAlltogether(model,trainDocs,valDocs,jointTraining,clueSpanLength,allowEventOnSameSpanStart,maxNumOfEventsOnSpan,maxMultipleArgUseAsTheme,clueValidationIterations,jointValidationIterations,eventOnlyValidationIterations,temperature,validationResetEachNumOfIteration)

    
    val writePrefixTraining = InitFile.initKeyValues("writeFinalTraining")
    if (writePrefixTraining != "null")
      trainDocs.foreach((d:Document) => Data.writeDocumentToFiles(d,writePrefixTraining+"/"+d.id+".txt",writePrefixTraining+"/"+d.id+".a1",writePrefixTraining+"/"+d.id+".a2.t1",false))
    val writePrefixVal = InitFile.initKeyValues("writeFinalValidation")
    if (valDocs != null && writePrefixVal != "null")
      valDocs.foreach((d:Document) => Data.writeDocumentToFiles(d,writePrefixVal+"/"+d.id+".txt",writePrefixVal+"/"+d.id+".a1",writePrefixVal+"/"+d.id+".a2.t1",false))

    Data.showAllFPEvents(valDocs,false,false,false,true)
	}


}