package cc.refectorie.bionlp

import io.Source
import collection.mutable.HashMap

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object InitFile {
  val initKeyValues = new HashMap[String,String]

  def initialize(filename:String) = {
    val iniFileLines = Source.fromFile(filename).getLines
    for((line:String) <- iniFileLines ; if (!line.startsWith("#") && line.contains("="))) {
      val split = line.split("=").map(x => x.trim.replaceAll("[\\n\\r]",""))
      initKeyValues += split(0) -> split(1)
    }
    debugFeaturesForTiny = if (InitFile.initKeyValues.contains("showFeaturesForTiny")) InitFile.initKeyValues("showFeaturesForTiny")=="true" else false

    featurecaching = if (InitFile.initKeyValues.contains("featurecaching")) InitFile.initKeyValues("featurecaching")=="true" else false

    val ngrams = if (InitFile.initKeyValues.contains("ngrams")) InitFile.initKeyValues("ngrams").toInt else 0
  
    pathngrams = if (ngrams == 0) {
    new Array[Int](0)
  } else if (ngrams == 1) {
    val tmp = new Array[Int](1)
    tmp(0) = 1
    tmp
  } else if (ngrams == 2) {
    val tmp = new Array[Int](2)
    tmp(0) = 1
    tmp(1) = 2
    tmp
  } else if (ngrams == 3) {
    val tmp = Array(1,2,3)
    tmp
  } else
    new Array[Int](0)

    val nprefix = if (InitFile.initKeyValues.contains("nprefix")) InitFile.initKeyValues("nprefix").toInt else 0

    pathnprefix = if (nprefix == 0) {
    new Array[Int](0)
  } else if (nprefix == 1) {
    val tmp = new Array[Int](1)
    tmp(0) = 1
    tmp
  } else if (nprefix == 2) {
    val tmp = new Array[Int](2)
    tmp(0) = 1
    tmp(1) = 2
    tmp
  } else if (nprefix == 3) {
    val tmp = Array(1,2,3)
    tmp
  } else
    new Array[Int](0)

    selftokenizedpath = (InitFile.initKeyValues("selftokenized")=="true")
    mcpath = (InitFile.initKeyValues("mc")=="true")
    evaluateOnTrainingAtEachIteration = InitFile.initKeyValues("evaluateOnTrainingAtEachIteration").toInt

    // in workflow for 0 and 1, here it is for 2
    useAll = InitFile.initKeyValues("useAll").toBoolean
    bindingHeuristic = InitFile.initKeyValues.getOrElse("bindingHeuristic","false").toBoolean
    useArgPairFeatures = InitFile.initKeyValues.getOrElse("useArgPairFeatures","true").toBoolean
    useNaiveDW = InitFile.initKeyValues.getOrElse("useNaiveDW","false").toBoolean
    relationalObjective = InitFile.initKeyValues.getOrElse("relationalObjective","false").toBoolean
    clueSpanLength = InitFile.initKeyValues("clueSpanLength").toInt
    cutoff = if (InitFile.initKeyValues.contains("cutoff")) InitFile.initKeyValues("cutoff").toInt else 0
    allowEventOnSameSpanStart = InitFile.initKeyValues("allowEventOnSameSpanStart").toBoolean // DOES NOT WORK
    temperature = InitFile.initKeyValues("temperature").toDouble
    fpWeight = InitFile.initKeyValues("fpWeight").toDouble
    argPairWeight = InitFile.initKeyValues.getOrElse("argPairWeight","0.0").toDouble
    trainTemperature = InitFile.initKeyValues("trainTemperature").toDouble
    clueTraining = if (InitFile.initKeyValues.contains("clueTraining")) InitFile.initKeyValues("clueTraining").toInt else 0
    jointTraining = if (InitFile.initKeyValues.contains("jointTraining")) InitFile.initKeyValues("jointTraining").toInt else 0
    eventAloneTraining = InitFile.initKeyValues("eventAloneTraining").toInt
    validation = InitFile.initKeyValues("validate").toBoolean
    maxNumOfEventsOnSpan = InitFile.initKeyValues("maxNumOfEventsOnSpan").toInt
    maxMultipleArgUseAsTheme = InitFile.initKeyValues("maxMultipleArgUseAsTheme").toInt
    validationResetEachNumOfIteration = InitFile.initKeyValues("validationResetEachNumOfIteration").toInt
    clueValidationIterations = if (InitFile.initKeyValues.contains("clueValidation")) InitFile.initKeyValues("clueValidation").toInt else 0
    jointValidationIterations = if (InitFile.initKeyValues.contains("jointValidation")) InitFile.initKeyValues("jointValidation").toInt else 0
    eventOnlyValidationIterations = InitFile.initKeyValues("eventAloneValidation").toInt
    folds = if (InitFile.initKeyValues.contains("folds")) InitFile.initKeyValues("folds").toInt else 8
    putSpansOnAllTokens = if (InitFile.initKeyValues.contains("putSpansOnAllTokens")) InitFile.initKeyValues("putSpansOnAllTokens").toBoolean else false  // DOES NOT WORK
    doTouch = if (InitFile.initKeyValues.contains("showTrueButNeverProposed")) InitFile.initKeyValues("showTrueButNeverProposed")=="true" else false
    showNeverProposed = InitFile.initKeyValues("showNeverProposed")=="true"
    ignoreregulations = if (InitFile.initKeyValues.contains("ignoreregulations")) InitFile.initKeyValues("ignoreregulations")=="true" else false
    writeIntermediateValidation = if (InitFile.initKeyValues.contains("writeIntermediateValidation")) InitFile.initKeyValues("writeIntermediateValidation") else null
    writeIntermediateTraining = if (InitFile.initKeyValues.contains("writeIntermediateTraining")) InitFile.initKeyValues("writeIntermediateTraining") else null
    allowChangeOfRegulationType = if (InitFile.initKeyValues.contains("allowChangeOfRegulationType")) InitFile.initKeyValues("allowChangeOfRegulationType")=="true" else false
    allowStealingOfArguments = if (InitFile.initKeyValues.contains("allowStealingOfArguments")) InitFile.initKeyValues("allowChangeOfRegulationType")=="true" else false
    allowChangeOfAllEventTypes = if (InitFile.initKeyValues.contains("allowChangeOfAllEventTypes")) InitFile.initKeyValues("allowChangeOfAllEventTypes")=="true" else false
    allowSplittingOfBinding = if (InitFile.initKeyValues.contains("allowSplittingOfBinding")) InitFile.initKeyValues("allowSplittingOfBinding")=="true" else false
    allowToExchangeTwoArgumentsInEvent = if (InitFile.initKeyValues.contains("allowToExchangeTwoArgumentsInEvent")) InitFile.initKeyValues("allowToExchangeTwoArgumentsInEvent")=="true" else false
    allowToExchangeArgumentsBetweenEvents = if (InitFile.initKeyValues.contains("allowToExchangeArgumentsBetweenEvents")) InitFile.initKeyValues("allowToExchangeArgumentsBetweenEvents")=="true" else false
    onlyExchangeBetweenEventsOfSameType = if (InitFile.initKeyValues.contains("onlyExchangeBetweenEventsOfSameType")) InitFile.initKeyValues("onlyExchangeBetweenEventsOfSameType")=="true" else false
    onlyExchangeBetweenEventsOfSameClass = if (InitFile.initKeyValues.contains("onlyExchangeBetweenEventsOfSameClass")) InitFile.initKeyValues("onlyExchangeBetweenEventsOfSameClass")=="true" else false
    punishArgF = if (InitFile.initKeyValues.contains("punishArgF")) InitFile.initKeyValues("punishArgF")=="true" else true
    filterforequivalent = if (InitFile.initKeyValues.contains("filterforequivalent")) InitFile.initKeyValues("filterforequivalent")=="true" else false
    useshortest = if (InitFile.initKeyValues.contains("useshortest")) InitFile.initKeyValues("useshortest")=="true" else false
    oc = if (InitFile.initKeyValues.contains("oc")) InitFile.initKeyValues("oc")=="true" else false
    connectHivenTokensToLongerSpan = if (InitFile.initKeyValues.contains("connectHivenTokensToLongerSpan")) InitFile.initKeyValues("connectHivenTokensToLongerSpan")=="true" else false
    semicolonAsBoundary = if (InitFile.initKeyValues.contains("semicolonAsBoundary")) InitFile.initKeyValues("semicolonAsBoundary")=="true" else false
    proposeSelfRegulation = if (InitFile.initKeyValues.contains("proposeSelfRegulation")) InitFile.initKeyValues("proposeSelfRegulation")=="true" else false
    lastWordGenesFeature = if (InitFile.initKeyValues.contains("lastWordGenesFeature")) InitFile.initKeyValues("lastWordGenesFeature")=="true" else false
    eventeventsharingargtemplate = if (InitFile.initKeyValues.contains("eventeventsharingargtemplate")) InitFile.initKeyValues("eventeventsharingargtemplate")=="true" else false
    distance = if (InitFile.initKeyValues.contains("distance")) InitFile.initKeyValues("distance")=="true" else false
    removeSentenceBreakingEvents = if (InitFile.initKeyValues.contains("removeSentenceBreakingEvents")) InitFile.initKeyValues("removeSentenceBreakingEvents")=="true" else true
    featuresOfArgumentsOfEventsOnSameSpan = if (InitFile.initKeyValues.contains("featuresOfArgumentsOfEventsOnSameSpan")) InitFile.initKeyValues("featuresOfArgumentsOfEventsOnSameSpan")=="true" else false
    docWideFeatures = if (InitFile.initKeyValues.contains("docWideFeatures")) InitFile.initKeyValues("docWideFeatures")=="true" else false
    sameClueString = if (InitFile.initKeyValues.contains("sameClueString")) InitFile.initKeyValues("sameClueString")=="true" else false
    sameClueSameTheme = if (InitFile.initKeyValues.contains("sameClueSameTheme")) InitFile.initKeyValues("sameClueSameTheme")=="true" else false
    sameClueSameArgs = if (InitFile.initKeyValues.contains("sameClueSameArgs")) InitFile.initKeyValues("sameClueSameArgs")=="true" else false
    writeFinalTraining = if (InitFile.initKeyValues.contains("writeFinalTraining")) InitFile.initKeyValues("writeFinalTraining") else null
    writeFinalValidation = if (InitFile.initKeyValues.contains("writeFinalValidation")) InitFile.initKeyValues("writeFinalValidation") else null
    veryCloseArgsFeature = if (InitFile.initKeyValues.contains("veryCloseArgsFeature")) InitFile.initKeyValues("veryCloseArgsFeature")=="true" else false
    noSameStringArgs =  if (InitFile.initKeyValues.contains("noSameStringArgs")) InitFile.initKeyValues("noSameStringArgs")=="true" else false
    noSpanList = if (InitFile.initKeyValues.contains("noSpanList")) InitFile.initKeyValues("noSpanList") else ""
    veryCloseArgsFeature2 = if (InitFile.initKeyValues.contains("veryCloseArgsFeature2")) InitFile.initKeyValues("veryCloseArgsFeature2")=="true" else false
    selftokenized = if (InitFile.initKeyValues.contains("selftokenized")) InitFile.initKeyValues("selftokenized")=="true" else true
    mc = if (InitFile.initKeyValues.contains("mc")) InitFile.initKeyValues("mc")=="true" else false
    fixTrees = if (InitFile.initKeyValues.contains("fixTrees")) InitFile.initKeyValues("fixTrees")=="true" else false
    headfeature = if (InitFile.initKeyValues.contains("headfeature")) InitFile.initKeyValues("headfeature")=="true" else false
    shuffledSampling = if (InitFile.initKeyValues.contains("shuffledSampling")) InitFile.initKeyValues("shuffledSampling")=="true" else false
    generalizePosNegRegulation = if (InitFile.initKeyValues.contains("generalizePosNegRegulation")) InitFile.initKeyValues("generalizePosNegRegulation")=="true" else false
    selfregulationtemplate = if (InitFile.initKeyValues.contains("selfregulationtemplate")) InitFile.initKeyValues("selfregulationtemplate")=="true" else false
    eventtemplate = if (InitFile.initKeyValues.contains("eventtemplate")) InitFile.initKeyValues("eventtemplate")=="true" else true
    syntactictemplate = if (InitFile.initKeyValues.contains("syntactictemplate")) InitFile.initKeyValues("syntactictemplate")=="true" else false
    genialist = if (InitFile.initKeyValues.contains("genialist")) InitFile.initKeyValues("genialist")=="true" else false
    genialistpath = if (InitFile.initKeyValues.contains("genialistpath")) InitFile.initKeyValues("genialistpath") else ""
    nootherfeaturesforselfregulation = if (InitFile.initKeyValues.contains("nootherfeaturesforselfregulation")) InitFile.initKeyValues("nootherfeaturesforselfregulation")=="true" else false
    useHeadForEval = if (InitFile.initKeyValues.contains("useHeadForEval")) InitFile.initKeyValues("useHeadForEval")=="true" else false
    dontAddDuplicates = if (InitFile.initKeyValues.contains("dontAddDuplicates")) InitFile.initKeyValues("dontAddDuplicates")=="true" else false
    dw2 = if (InitFile.initKeyValues.contains("dw2")) InitFile.initKeyValues("dw2")=="true" else false
    dw3 = if (InitFile.initKeyValues.contains("dw3")) InitFile.initKeyValues("dw3")=="true" else false
    dw3recursive = if (InitFile.initKeyValues.contains("dw3recursive")) InitFile.initKeyValues("dw3recursive")=="true" else false
    dw3simple = if (InitFile.initKeyValues.contains("dw3simple")) InitFile.initKeyValues("dw3simple")=="true" else false
    sameClue = if (InitFile.initKeyValues.contains("sameClue")) InitFile.initKeyValues("sameClue")=="true" else false
    mainClueTokenHeuristics = if (InitFile.initKeyValues.contains("mainClueTokenHeuristics")) InitFile.initKeyValues("mainClueTokenHeuristics")=="true" else false
    onlySampleKnownClues = if (InitFile.initKeyValues.contains("onlySampleKnownClues")) InitFile.initKeyValues("onlySampleKnownClues")=="true" else false
    sameArg = if (InitFile.initKeyValues.contains("sameArg")) InitFile.initKeyValues("sameArg")=="true" else false
    useMira = if (InitFile.initKeyValues.contains("useMira")) InitFile.initKeyValues("useMira")=="true" else false
    parentChild = if (InitFile.initKeyValues.contains("parentChild")) InitFile.initKeyValues("parentChild")=="true" else false
    miGeneFrequency = if (InitFile.initKeyValues.contains("miGeneFrequency")) InitFile.initKeyValues("miGeneFrequency")=="true" else false
    miGeneFrequencyEvents = if (InitFile.initKeyValues.contains("miGeneFrequencyEvents")) InitFile.initKeyValues("miGeneFrequencyEvents")=="true" else false
    miGeneFirst = if (InitFile.initKeyValues.contains("miGeneFirst")) InitFile.initKeyValues("miGeneFirst")=="true" else false
    miGeneLast = if (InitFile.initKeyValues.contains("miGeneLast")) InitFile.initKeyValues("miGeneLast")=="true" else false
    miGeneFirstEvent = if (InitFile.initKeyValues.contains("miGeneFirstEvent")) InitFile.initKeyValues("miGeneFirstEvent")=="true" else false
    miGeneLastEvent = if (InitFile.initKeyValues.contains("miGeneLastEvent")) InitFile.initKeyValues("miGeneLastEvent")=="true" else false
    miGene = miGeneFrequency || miGeneFrequencyEvents || miGeneFirst || miGeneLast || miGeneFirstEvent || miGeneLastEvent
    dwSameEventTypeOnClue = if (InitFile.initKeyValues.contains("dwSameEventTypeOnClue")) InitFile.initKeyValues("dwSameEventTypeOnClue")=="true" else false
    guessEquivalent = if (InitFile.initKeyValues.contains("guessEquivalent")) InitFile.initKeyValues("guessEquivalent")=="true" else false
  }

	def main(args: Array[String]) : Unit = {
    InitFile.initialize("/home/rklinger/work/mercurial/refectorie/proj/bionlp/init/default.ini")
    for(key <- InitFile.initKeyValues.keySet) {
      System.err.println(key+" -> "+InitFile.initKeyValues(key))
    }
  }

  // defaults
  var pathngrams = new Array[Int](0)
  var pathnprefix = new Array[Int](0)
  var debugFeaturesForTiny = false
  var featurecaching = false
  var selftokenizedpath = false
  var mcpath = false
  var evaluateOnTrainingAtEachIteration = 1
  var useAll = true
  var dontAddDuplicates = false
  var mainClueTokenHeuristics = false

  var bindingHeuristic = false

  var fpWeight = 1.0
  var argPairWeight = 1.0
  var useMira = false
  var relationalObjective = false
  var useNaiveDW = false
  var useArgPairFeatures = true


  var clueSpanLength = 1
  var allowEventOnSameSpanStart = false
  var temperature = 0.0001
  var trainTemperature = 0.0001
  var clueTraining = 10
  var jointTraining = 10
  var eventAloneTraining = 10
  var validation = true
  var maxNumOfEventsOnSpan = 2 // means 1, but correct in workflow2
  var maxMultipleArgUseAsTheme = 2 // means 1
  var validationResetEachNumOfIteration = 2
  var clueValidationIterations = 2
  var jointValidationIterations = 2
  var eventOnlyValidationIterations = 2
  var onlySampleKnownClues = false
  var folds = 7
  var cutoff = 1
  var putSpansOnAllTokens = false
  var useHeadForEval = false
  var doTouch = false
  var showNeverProposed = false
  var ignoreregulations = false
  var writeIntermediateValidation:String = null
  var writeIntermediateTraining:String = null
  var allowChangeOfRegulationType = false
  var allowChangeOfAllEventTypes = false
  var allowSplittingOfBinding = false
  var allowStealingOfArguments = false
  var allowToExchangeTwoArgumentsInEvent = false
  var punishArgF = true
  var allowToExchangeArgumentsBetweenEvents = false
  var onlyExchangeBetweenEventsOfSameType = false
  var onlyExchangeBetweenEventsOfSameClass = false
  var filterforequivalent = false
  var useshortest = false
  var oc = false
  var connectHivenTokensToLongerSpan = false
  var semicolonAsBoundary = false
  var proposeSelfRegulation = false
  var lastWordGenesFeature = false
  var eventeventsharingargtemplate = false
  var eventtemplate = true
  var syntactictemplate = false
  var distance = false
  var removeSentenceBreakingEvents = true
  var featuresOfArgumentsOfEventsOnSameSpan = false
  var writeFinalTraining:String = null
  var writeFinalValidation:String = null
  var docWideFeatures = false
  var sameClueString = false
  var sameClueSameTheme = false
  var sameClueSameArgs = false
  var veryCloseArgsFeature = false
  var veryCloseArgsFeature2 = false
  var noSameStringArgs = false
  var noSpanList = ""
  var selftokenized = true
  var fixTrees = false
  var mc = false
  var headfeature = false
  var shuffledSampling = false
  var generalizePosNegRegulation = false
  var selfregulationtemplate = false
  var genialist = false
  var genialistpath:String = ""
  var nootherfeaturesforselfregulation = false
  var dw2 = false
  var dw3 = false
  var dw3simple = false
  var dw3recursive = false
  var dwSameEventTypeOnClue = false
  var sameClue = false
  var sameArg = false
  var parentChild = false
  var miGeneFrequency = false
  var miGeneFrequencyEvents = false
  var miGeneFirst = false
  var miGeneLast = false
  var miGeneFirstEvent = false
  var miGeneLastEvent = false
  var miGene = false // automatically from the ones above, not in config file!
  var guessEquivalent = false
}
