package cc.refectorie.bionlp

import cc.factorie._
import cc.refectorie.bionlp.TemplatesForPieceWise._
import cc.refectorie.bionlp.PieceWiseSampler.MetaSampler
import collection.immutable.HashSet
import cc.refectorie.bionlp.TemplatesOverSentences.{EventsInDifferentSentencesFeatureVector, EventsInDifferentSentencesTemplate}
import io.Source
import collection.mutable.{ArrayBuffer, HashMap}
import cc.refectorie.bionlp.SelfRegulationTemplates.{SelfRegulationFeatureVector, SelfRegulationTemplate}
import cc.refectorie.bionlp.DictionaryTemplates.{DictPrecision, DictWithPrecisionTemplate}
import cc.refectorie.bionlp.MCEventTemplates._
import java.io.{PrintStream, FileInputStream, BufferedInputStream, File}

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object Workflow2 {
  def showNeverTouched(corpus: Array[Document], iteration: Int) {
    if (InitFile.doTouch) {
      val neverTouchedEvents = for (doc <- corpus; sentence: Sentence <- doc; event: Event <- sentence.trueSentence.events; if (!event.touchedFromProposer)) yield event
      System.err.println("Never proposed in iteration " + iteration + ": " + neverTouchedEvents.length)
      val countType = new HashMap[String, Int]
      for (event: Event <- neverTouchedEvents) {
        val et = event.eventType.value
        if (!countType.contains(et)) countType += et -> 1
        else countType += et -> (countType(et) + 1)
        if (InitFile.showNeverProposed) {
          //if (!et.contains("egulation")) {
          val varSentence = event.clue.sentence.asInstanceOf[TrueSentence].variableSentence
          val clueExisting = varSentence.spans.exists((s: Span) => s.start == event.clue.start && s.length == event.clue.length)
          System.err.print(event.outputFormatE)
          System.err.print("\t\t" + (if (clueExisting) "Exist    " else "Not Exist") + " : " + event.outputFormatT)
          event.arguments.foreach(x => {
            val argExisting = varSentence.spans.exists((s: Span) => s.start == x.entity.span.start && s.length == x.entity.span.length);
            System.err.print("\t\t" + (if (argExisting) "Exist    " else "Not Exist") + " : " + (if (x.entity.isInstanceOf[Gene]) x.entity.asInstanceOf[Gene].outputFormat else x.entity.asInstanceOf[Event].outputFormatE))
          })
          //}
        }
      }
      for (et <- countType.keySet)
        System.err.println(et + ":\t" + countType(et))
    }
  }

  def train(model: Model, trainDocs: Array[Document], valDocs: Array[Document]): Unit = {
    var obj = new Model
    if (InitFile.relationalObjective) {
      obj += new TemplateWithStatistics1[Event] {
        def score(s: Stat) = {
          val event: Event = s.s1
          val eval = event.relationalEval
          val tp = eval.clueTypeTP + eval.clueArgTP + InitFile.argPairWeight * eval.argPairTP
          val fp = eval.clueTypeFP + eval.clueArgFP + InitFile.argPairWeight * eval.argPairFP
          tp - InitFile.fpWeight * fp
        }
      }
      obj += new TemplateWithStatistics1[Sentence] {
        def score(s: Stat) = {
          val sentence: Sentence = s.s1.asInstanceOf[Sentence]
          val (tp, fp, fn, p, r, f) = sentence.eval(true, true, false, false) // checking type and theme, not arguments
          tp - InitFile.fpWeight * fp // best
        }
      }
//      obj += new TemplateWithStatistics1[Event] {
//        def score(s: Stat) = {
//          val e: Event = s.s1.asInstanceOf[Event]
//          if (InitFile.useMira) {
//            if (!e.present) 0 else {
//              val (typeTp, typeFp) = if (e.isATrueEventInSentence(true, false, false, null))
//                (1.0, 0.0) else (0.0, 1.0)
//              val (tp, fp, fn, p, r, f) = e.evalFMeasureOfArguments()
//
//              (tp + typeTp) - InitFile.fpWeight * (fp + typeFp)
//            }
//          } else {
//            if (!e.present)
//              0.0
//            else {
//              //val good = e.isATrueEventInSentence(true,true,false,null)
//              //val goodSpan = e.isATrueEventInSentence(false,false,false,null)
//              val goodType = e.isATrueEventInSentence(true, false, false, null)
//              //val goodTypeAndTheme = e.isATrueEventInSentence(true,true,false,null) // todo what about multiple themes?
//              //val goodTypeAndArgs = e.isATrueEventInSentence(true,false,true,null)
//              if (goodType) {
//                val (tp, fp, fn, p, r, f) = e.evalFMeasureOfArguments()
//                val punish = if (fp > 0) 0.01 else 0.0
//                if (InitFile.punishArgF) {
//                  1.0 + f - punish // before it was only f (._6) // TODO, check, it is unclear, if it is better.
//                } else
//                  f
//              }
//              else
//                -1.0
//            }
//          }
//        }
//      }

    } else {
      val fpWeight = if (InitFile.useMira) InitFile.fpWeight else 1.0
      if (!InitFile.useMira) {
        obj += new TemplateWithStatistics1[Sentence] {
          def score(s: Stat) = {
            val sentence: Sentence = s.s1.asInstanceOf[Sentence]
            val (tp, fp, fn, p, r, f) = sentence.eval(true, true, false, false) // checking type and theme, not arguments
            tp - fpWeight * fp // best
          }
        }
      }
      // measure the event, type and theme: HAS THE PROBLEM OF MULTIPLE IDENTICAL ENTITIES
      obj += new TemplateWithStatistics1[Event] {
        def score(s: Stat) = {
          val e: Event = s.s1.asInstanceOf[Event]
          if (InitFile.useMira) {
            if (!e.present) 0 else {
              val (typeTp, typeFp) = if (e.isATrueEventInSentence(true, false, false, null))
                (1.0, 0.0) else (0.0, 1.0)
              val (tp, fp, fn, p, r, f) = e.evalFMeasureOfArguments()

              (tp + typeTp) - InitFile.fpWeight * (fp + typeFp)
            }
          } else {
            if (!e.present)
              0.0
            else {
              //val good = e.isATrueEventInSentence(true,true,false,null)
              //val goodSpan = e.isATrueEventInSentence(false,false,false,null)
              val goodType = e.isATrueEventInSentence(true, false, false, null)
              //val goodTypeAndTheme = e.isATrueEventInSentence(true,true,false,null) // todo what about multiple themes?
              //val goodTypeAndArgs = e.isATrueEventInSentence(true,false,true,null)
              if (goodType) {
                val (tp, fp, fn, p, r, f) = e.evalFMeasureOfArguments()
                val punish = if (fp > 0) 0.01 else 0.0
                if (InitFile.punishArgF) {
                  1.0 + f - punish // before it was only f (._6) // TODO, check, it is unclear, if it is better.
                } else
                  f
              }
              else
                -1.0
            }
          }
        }
      }
    }
    // GradientAscentUpdates /// best
    //ConfidenceWeightedUpdates // leads to very low precision
    // MIRAUpdates // not so bad, but still half of GradientAscentUpdates
    //SecondOrderGradientAscentUpdates // bad
    //StdDevConfidenceWeightedUpdates // bad
    //AROWUpdates // leads to very low precision
    val metaSam = if (InitFile.useMira)
      new MetaSampler(model, obj) with SampleRank with MIRAUpdates with ParameterAveraging else
      new MetaSampler(model, obj) with SampleRank with GradientAscentUpdates with ParameterAveraging
    metaSam.eventsWithThemeOverClues.temperature = InitFile.trainTemperature
    metaSam.exchangeArgs.temperature = InitFile.trainTemperature
    metaSam.themesToEvents.temperature = InitFile.trainTemperature
    metaSam.temperature = InitFile.trainTemperature

    System.err.println("Putting spans on all tokens")
    val allSentences = for (d <- trainDocs; s <- d) yield s
    val count = spanOnAllSentences(allSentences)
    val allSpans = shuffleOrNot(for (sent <- allSentences; span <- sent.spans; if (span.isOfEvent)) yield span)

    System.err.println("Starting sampling over spans...")
    for (iteration <- 1 to InitFile.eventAloneTraining) {
      System.err.println("Iteration " + iteration)
      var c = 0

      for (span: Span <- allSpans) {
        if (c % 1000 == 0) System.err.print("Done: " + c + "/" + count + "\r")
        c += 1
        if (!span.sentence.genes.isEmpty) {
          metaSam.process(span) // put events with type on the span, remove existing events
          for (eventOnSpan: Event <- span.events) {
            metaSam.process(eventOnSpan) // add theme or remove event again
          }
        }
      }
      // exchange arguments
      for (sentence <- allSentences) {
        if (InitFile.allowToExchangeArgumentsBetweenEvents) {
          for (e1 <- sentence.events; e2 <- sentence.events; if (e1.hashCode < e2.hashCode) && e1 != e2) { // the latter just to be sure
            if ((InitFile.onlyExchangeBetweenEventsOfSameType && e1.eventType.value == e2.eventType.value) || // type is identical
                    (InitFile.onlyExchangeBetweenEventsOfSameClass) && (e1.eventType.value.contains("egulation") == e2.eventType.value.contains("egulation")) || // both from regulation or not
                    ((!InitFile.onlyExchangeBetweenEventsOfSameType) && (!InitFile.onlyExchangeBetweenEventsOfSameClass)) // all is allowed
            )
              metaSam.process(new EventPair(e1, e2))
          }
        }
      }
      System.err.println
      Data.evaluateCorpusOnClues("Evaluation clues        ", iteration, trainDocs)
      Data.evaluateCorpusOnEvents("Evaluation event pos   ", iteration, trainDocs, false, false, false)
      Data.evaluateCorpusOnEvents("Evaluation event type  ", iteration, trainDocs, true, false, false)
      Data.evaluateCorpusOnEvents("Evaluation event theme ", iteration, trainDocs, true, true, false)
      Data.evaluateCorpusOnEvents("Evaluation event args  ", iteration, trainDocs, true, false, true)

      if (InitFile.writeIntermediateTraining != null) {
        Data.writeCorpus(trainDocs, InitFile.writeIntermediateTraining + "/" + iteration + "-joint", false)
        WhatsWrongOutputGenerator.write(trainDocs,
          new PrintStream(InitFile.writeIntermediateTraining + "/guess" + iteration + ".ww"))
        WhatsWrongOutputGenerator.writeGold(trainDocs,
          new PrintStream(InitFile.writeIntermediateTraining + "/gold" + iteration + ".ww"))
        val weightOut = new PrintStream(InitFile.writeIntermediateTraining + "/" + iteration + ".weights")
        for (feature <- SyntacticPatternTemplate.orderedWeights;
             if (feature.count > SyntacticPatternTemplate.cutoff))
          weightOut.println("%-30s %-5d %-7f".format(feature.feature, feature.count, feature.weight))
        weightOut.close
      }

      //      println("EventPair Same Arg weights:\n%s".format(EventPairTemplates.SameArgEventsTemplate.weightsToString.mkString("\n")))
      //      println("EventPair Same Clue weights:\n%s".format(EventPairTemplates.SameClueEventsTemplate.weightsToString.mkString("\n")))
      //      println("Parent Child weights:\n%s".format(EventPairTemplates.ParentChildEventsTemplate.weightsToString.mkString("\n")))

      showNeverTouched(trainDocs, iteration)

      // RESET AND VALIDATION
      if (iteration % InitFile.validationResetEachNumOfIteration == 0) {
        //reset
        if (iteration < InitFile.eventAloneTraining) {
          System.err.println("Reset (each " + InitFile.validationResetEachNumOfIteration + " iteration)")
          trainDocs.foreach((d: Document) => d.reset(false))
        }
        //validation
        if (valDocs != null) {
          valDocs.foreach((d: Document) => d.reset(true))
          metaSam.setWeightsToAverage
          evaluation(model, valDocs, iteration)
          metaSam.unsetWeightsToAverage

        }
      }
    }
  }

  def shuffleOrNot(allSpans: Seq[Span]): ArrayBuffer[Span] = {
    val spanList = java.util.Arrays.asList(allSpans.toArray: _*)
    if (InitFile.shuffledSampling) {
      java.util.Collections.shuffle(spanList, new java.util.Random(1))
    }
    val r = new ArrayBuffer[Span]
    for (i <- 0 until spanList.size) r += spanList.get(i)
    r
  }

  def spanOnAllSentences(allSentences: Seq[Sentence]): Int = {
    var count = 0
    val noSpan = new scala.collection.mutable.HashSet[String]
    if (InitFile.noSpanList != "") {
      val fis = new FileInputStream(InitFile.noSpanList)
      val bis = new BufferedInputStream(fis)
      val source = Source.fromInputStream(bis)
      val lines = source.getLines
      for (l <- lines) noSpan += l.trim
      bis.close
      fis.close
      System.err.println("Filtering spans with a list of length " + noSpan.size)
    }

    for (sentence: Sentence <- allSentences) {
      //System.err.println("Sentence: "+sentence.toString(true))
      for (token: Token <- sentence;
           if (sentence.spansStartingAt(token.sentencePos).isEmpty && (!sentence.genes.flatMap(_.span).contains(token))); // if make longer spans, do not make a new span starting together with a cont. of a long one
           if (!noSpan.contains(token.word))) {
        if (!(InitFile.connectHivenTokensToLongerSpan && sentence.spans.exists(s => s.contains(token)))) {
          for (clueLength <- 1 to InitFile.clueSpanLength; if (token.sentencePos + clueLength - 1 < sentence.length)) {
            var debugSpan: Span = null
            if (InitFile.connectHivenTokensToLongerSpan && token.hasNext && token.next.word == "-") {
              debugSpan = new Span(sentence, token.sentencePos, 3, null, "  T999")(null) // make one span out of this token, the following hiven and the next token
            } else if (token.word.length > 1) {
              debugSpan = new Span(sentence, token.sentencePos, clueLength, null, "T999")(null)
            }
            //System.err.println("Span made: "+debugSpan.stringrepr)
            count += 1
          }
        }
      }
    }
    System.err.println("There are " + count + " spans added.")
    count
  }

  def evaluation(model: Model, valDocs: Array[Document], trainiteration: Int) { // TODO add spans before starting
    val metaSam = new MetaSampler(model, null)
    val allSentences = for (d <- valDocs; s <- d) yield s
    val count = spanOnAllSentences(allSentences)
    val allSpans = shuffleOrNot(for (sent <- allSentences; span <- sent.spans; if (span.isOfEvent)) yield span)

    for (iteration <- 1 to InitFile.eventOnlyValidationIterations) {
      System.err.println("Iteration " + iteration)
      for (span <- allSpans) {
        metaSam.process(span) // put events with type on the span, remove existing events
        for (eventOnSpan: Event <- span.events) {
          metaSam.process(eventOnSpan) // add theme or remove event again
        }
      }
      for (sentence <- allSentences) {
        if (InitFile.allowToExchangeArgumentsBetweenEvents) {
          for (e1 <- sentence.events; e2 <- sentence.events; if (e1.hashCode < e2.hashCode) && e1 != e2) { // the latter just to be sure
            if ((InitFile.onlyExchangeBetweenEventsOfSameType && e1.eventType.value == e2.eventType.value) || // type is identical
                    (InitFile.onlyExchangeBetweenEventsOfSameClass) && (e1.eventType.value.contains("egulation") == e2.eventType.value.contains("egulation")) || // both from regulation or not
                    ((!InitFile.onlyExchangeBetweenEventsOfSameType) && (!InitFile.onlyExchangeBetweenEventsOfSameClass)) // all is allowed
            )
              metaSam.process(new EventPair(e1, e2))
          }
        }
      }
      Data.evaluateCorpusOnClues("\tEvaluation clues        ", iteration, valDocs)
      Data.evaluateCorpusOnEvents("\tEvaluation event pos   ", iteration, valDocs, false, false, false)
      Data.evaluateCorpusOnEvents("\tEvaluation event type  ", iteration, valDocs, true, false, false)
      Data.evaluateCorpusOnEvents("\tEvaluation event theme ", iteration, valDocs, true, true, false)
      Data.evaluateCorpusOnEvents("\tEvaluation event args  ", iteration, valDocs, true, false, true)
      if (InitFile.writeIntermediateValidation != null) {
        Data.writeCorpus(valDocs, InitFile.writeIntermediateValidation + "/" + trainiteration + "-" + iteration + "-joint", false)
        WhatsWrongOutputGenerator.write(valDocs,
          new PrintStream(InitFile.writeIntermediateValidation + "/guess" + iteration + "-" + trainiteration + ".ww"))
        WhatsWrongOutputGenerator.writeGold(valDocs,
          new PrintStream(InitFile.writeIntermediateValidation + "/gold" + iteration + "-" + trainiteration + ".ww"))

      }
    }
  }


  def main(args: Array[String]): Unit = {
    System.err.println("Starting training and testing workflow 2 (the new one!)...")
    System.err.println("Parameters specified: " + args.length)
    args.foreach(x => System.err.println(x))
    System.err.println
    if (args.length != 1 || !(new File(args(0)).exists)) {
      System.err.println("You need to specify an .ini file (as in the init folder).")
      System.exit(1)
    }

    InitFile.initialize(args(0))

    Domain[EventFeatureVector].maxSize = 3000000
    if (InitFile.eventeventsharingargtemplate) Domain[EventEventSharingArgFeatureVector].maxSize = 2000000
    if (InitFile.featuresOfArgumentsOfEventsOnSameSpan) Domain[SimilarEventDifferentArgFeatureVector].maxSize = 2000000
    if (InitFile.docWideFeatures) Domain[EventsInDifferentSentencesFeatureVector] maxSize = 2000000
    if (InitFile.selfregulationtemplate) Domain[SelfRegulationFeatureVector].maxSize = 2000000
    if (InitFile.genialist) Domain[DictPrecision].maxSize = 2000000
    if (InitFile.dw2) Domain[MCFeatureVector].maxSize = 200000
    if (InitFile.dw3simple) Domain[MCArgFeatureVector].maxSize = 200000
    if (InitFile.dw3recursive) Domain[MCArgRecFeatureVector].maxSize = 200000
    if (InitFile.dwSameEventTypeOnClue) Domain[ClueHasSameEventTypeFeatureVector].maxSize = 200000

    // the model
    val model = new Model
    if (InitFile.eventtemplate)
      model += (new EventTemplate).init
    if (InitFile.eventeventsharingargtemplate)
      model += (new EventEventSharingArgTemplate).init
    if (InitFile.featuresOfArgumentsOfEventsOnSameSpan)
      model += (new SimilarEventDifferentArgTemplate).init
    if (InitFile.docWideFeatures)
      model += (new EventsInDifferentSentencesTemplate).init
    if (InitFile.selfregulationtemplate)
      model += (new SelfRegulationTemplate).init
    if (InitFile.genialist)
      model += (new DictWithPrecisionTemplate(InitFile.genialistpath)).init
    if (InitFile.dw2)
      model += (new MCEventsTemplate).init
    if (InitFile.dw3simple)
      model += (new MCArgumentTemplate).init
    if (InitFile.dw3recursive)
      model += (new MCArgumentRecTemplate).init
    if (InitFile.dwSameEventTypeOnClue)
      model += (new ClueHasSameEventTypeTemplate).init
    if (InitFile.useNaiveDW)
      model += EventPairTemplates.NaiveDocumentWideTemplate

    if (InitFile.dontAddDuplicates)
      model += SyntacticPatternTemplate.ArgPairTemplate

    if (InitFile.parentChild) {
      //      System.err.println("Collecting parent child featuresWithStats")
      //      EventPairTemplates.ParentChildEventsTemplate.collectGoldFeatures(trainDocs)
      //      System.err.println("Collected %d featuresWithStats".format(Domain[EventPairTemplates.ParentChildFeatures].size))
      model += EventPairTemplates.ParentChildEventsTemplate
    }
    if (InitFile.sameClue) {
      //      System.err.println("Collecting same clue featuresWithStats")
      //      EventPairTemplates.SameClueEventsTemplate.collectGoldFeatures(trainDocs)
      //      System.err.println("Collected %d featuresWithStats".format(Domain[EventPairTemplates.SameClueFeatures].size))
      model += EventPairTemplates.SameClueEventsTemplate
    }
    if (InitFile.sameArg) {
      //      System.err.println("Collecting same arg featuresWithStats")
      //      EventPairTemplates.SameArgEventsTemplate.collectGoldFeatures(trainDocs)
      //      System.err.println("Collected %d featuresWithStats".format(Domain[EventPairTemplates.SameArgFeatures].size))
      model += EventPairTemplates.SameArgEventsTemplate
    }

    var trainDocs1 = Data.readCorpus(InitFile.initKeyValues("trainfile"),false)
    var valDocs1 = if (InitFile.validation && InitFile.useAll) Data.readCorpus(InitFile.initKeyValues("valfile"),InitFile.guessEquivalent) else null
    Data.checkEntityHomogenityOnSpans = true
    // separating training and validation set
    val (trainDocs, valDocs) = if (InitFile.useAll)
      (trainDocs1, valDocs1)
    else Data.combineSubSampleToTrainingAndValidation(Data.cvSets(trainDocs1, InitFile.folds), 0)

    if (InitFile.syntactictemplate) {
      SyntacticPatternTemplate.cutoff = InitFile.cutoff
      SyntacticPatternTemplate.collectFeatures(trainDocs)
      model += SyntacticPatternTemplate
    }

    // extract gazateer
    for (doc <- trainDocs; s <- doc; span: Span <- s.trueSentence.spans; if (span.isOfEvent)) Data.clueGazateer.addEntry(span.map(_.word).mkString(" "))
    for (doc <- trainDocs; s <- doc; span: Span <- s.trueSentence.spans; if (span.isOfEvent)) Data.clueGazateer.addEntry(span.map(_.stem).mkString(" "))
    System.err.println("Gazateer: " + Data.clueGazateer.size + " entries.")
    for (doc <- trainDocs; s <- doc; span: Span <- s.trueSentence.spans; if (span.isOfEvent))
      SyntacticPatternTemplate.AllPossibleClues.add(span.mainClueToken)
    System.err.println("All clue stems: " + SyntacticPatternTemplate.AllPossibleClues.stems.size + " entries.")

    for (doc <- trainDocs; s <- doc; span: Span <- s.trueSentence.spans; if (span.isOfEvent)) for (token <- span) Data.clueTokenGazateer.addEntry(token.stem)
    // type dep gazateer
    for (doc <- trainDocs; s <- doc; span: Span <- s.trueSentence.spans; if (span.isOfEvent)) {
      for (e <- span.events) {
        val typ = e.eventType.value
        if (!Data.cluePerTypeGazateer.contains(typ)) Data.cluePerTypeGazateer += typ -> new HashSet[String]
        Data.cluePerTypeGazateer(typ).addEntry(span.map(_.word).mkString(" "))
        Data.cluePerTypeGazateer(typ).addEntry(span.map(_.stem).mkString(" "))
        if (typ.contains("egulation")) { // all regulations together
          if (!Data.cluePerTypeGazateer.contains("allreg")) Data.cluePerTypeGazateer += "allreg" -> new HashSet[String]
          Data.cluePerTypeGazateer("allreg").addEntry(span.map(_.word).mkString(" "))
          Data.cluePerTypeGazateer("allreg").addEntry(span.map(_.stem).mkString(" "))
        }
      }
      Data.clueGazateer.addEntry(span.map(_.stem).mkString(" "))
    }
    for (typInGaz <- Data.cluePerTypeGazateer.keySet) {
      System.err.println("Gazateer for " + typInGaz + ": " + Data.cluePerTypeGazateer(typInGaz).size + " entries.")
    }

    System.err.println("Train/Validation Set Size: " + trainDocs.size + "/" + (if (valDocs == null) 0 else valDocs.size))

    System.err.println("Model: %s".format(model.mkString(",")))

    // sampling
    train(model, trainDocs, valDocs)
    //    InitFile.showNeverProposed=true
    //    showNeverTouched(trainDocs,-999)

    val writePrefixTraining = InitFile.writeFinalTraining
    if (writePrefixTraining != null) {
      Data.writeCorpus(trainDocs, writePrefixTraining, false)
    }
    val writePrefixVal = InitFile.writeFinalValidation
    if (valDocs != null && writePrefixVal != null) {
      Data.writeCorpus(valDocs, writePrefixVal, false)
    }

    //Data.showAllFPEvents(trainDocs,false,false,true,false)
    //Data.showAllResultsPerSentence(trainDocs,true,true,false,false,true)
  }


}