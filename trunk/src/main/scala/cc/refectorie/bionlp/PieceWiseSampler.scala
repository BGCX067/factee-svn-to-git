package cc.refectorie.bionlp

import cc.factorie.{ProposalSampler, Model}
import cc.refectorie.bionlp._
import collection.mutable.ArrayBuffer

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object PieceWiseSampler {
  class MetaSampler(val model: Model, obj: Model) extends ProposalSampler[Any] {
    val eventsWithThemeOverClues = new EventsWithThemeOverClues(model, obj)
    val themesToEvents = new ThemesToEvents(model, obj)
    val exchangeArgs = new ExchangeArguments(model, obj)

    def proposals(x: Any): Seq[cc.factorie.Proposal] = {
      x match {
        case x: Span => eventsWithThemeOverClues.proposals(x)
        case x: Event => themesToEvents.proposals(x)
        case x: EventPair => exchangeArgs.proposals(x)
      }
    }

    def objective = obj

    override def preProcessHook(context: Any): Any = {
      if (context.isInstanceOf[Span]) {
        val span = context.asInstanceOf[Span]
        if (span.sentence.genes.isEmpty || InitFile.onlySampleKnownClues &&
                !SyntacticPatternTemplate.AllPossibleClues.hasStem(span.mainClueToken.stem))
          null else context

      } else context

    }
  }

  class ExchangeArguments(val model: Model, obj: Model) extends ProposalSampler[EventPair] {
    def objective = obj
    temperature = InitFile.temperature

    if (false) {
      this.proposalsHooks += {
        (proposals: Seq[cc.factorie.Proposal]) => {
          proposals.foreach((x: cc.factorie.Proposal) => System.err.println("Diff=" + x.diff + "; AccScore=" + x.acceptanceScore + "; ModScore=" +
                  x.modelScore + "; ObjScore=" + x.objectiveScore)
            )
        }
      }
    }

    def proposals(eventPair: EventPair): Seq[cc.factorie.Proposal] = { // TODO implement between all and only between those of same type
      val e1 = eventPair.e1
      val e2 = eventPair.e2
      //System.err.println("Exchanging between "+eventPair)
      val sentence = eventPair.e1.clue.sentence
      val choices = new ArrayBuffer[Choice]
      // nothing is allowed
      choices += Choice(1.0, diff => {})

      // through all arguments, for each fitting pair
      // exchange between both events, but only themes with themes and causes with causes. Shuffling in an event itself is done somewhere else
      for ((a1: Argument) <- e1.arguments; (a2: Argument) <- e2.arguments; if (a1.roleType.value == a2.roleType.value)) {
        if ((a1.entity != a2.entity) && // that would do nothing
                (a1.entity.span != e2.span) && (a2.entity.span != e1.span)) { // do not put an event on myself to myself
          if (!(InitFile.semicolonAsBoundary && (Data.isSymbolBetweenTokens(";", e1.clue.first, a2.entity.span.first) || Data.isSymbolBetweenTokens(";", e2.clue.first, a1.entity.span.first)))) {
            val roleType = a1.roleType.value
            choices += Choice(1.0, diff => {
              //System.err.println(eventPair)
              e1.removeEntityAsArgument(a1)(diff)
              e2.removeEntityAsArgument(a2)(diff)
              e1.addEntityAsArgument(a2.entity, roleType)(diff)
              e2.addEntityAsArgument(a1.entity, roleType)(diff)
              //System.err.println(eventPair)
            })
          }
        }
      }

      val proposals = for (choice: Choice <- choices.toList) yield
        choice.asProposalWithScoreAcceptance(model, obj, temperature)
      proposals
    }
  }

  class EventsWithThemeOverClues(val model: Model, obj: Model) extends ProposalSampler[Span] {
    def objective = obj

    temperature = InitFile.temperature

    if (false) {
      this.proposalsHooks += {
        (proposals: Seq[cc.factorie.Proposal]) => {
          proposals.foreach((x: cc.factorie.Proposal) => System.err.println("Diff=" + x.diff + "; AccScore=" + x.acceptanceScore + "; ModScore=" +
                  x.modelScore + "; ObjScore=" + x.objectiveScore)
            )
        }
      }
    }

    def proposals(clue: Span): Seq[cc.factorie.Proposal] = {
      if (clue.isOfGene) throw new Exception("Do not sample over gene spans! I warn you!")
      val sentence = clue.sentence
      val choices = new ArrayBuffer[Choice]

      // only if there are not too many here, add an event
      // get all events from overlapping spans, for the case we allow different lengths
      val numOfOverlappingEvents = if (InitFile.clueSpanLength > 1)
        (for (s <- clue.sentence.spans; e <- s.events; if (!s.overlaps(clue))) yield e).length
      else
        clue.events.length
      if (numOfOverlappingEvents >= InitFile.maxNumOfEventsOnSpan) {
        //System.err.println("Not adding due to "+clue.events.length+" events here.")
      } else {
        // make events on each span, not allowed to do nothing
        for (tn <- 0 to 8) {
          choices += Choice(1.0, diff => {sentence.addNewEvent(-1, -1, Data.possibleEventTypes(tn), clue)(diff)})
        }
      }
      // if there is one, we could remove the whole event
      if (clue.events.length > 0) {
        for (eventToRemove <- clue.events) {
          //System.err.println("removing... "+eventToRemove.outputFormatE+"/"+eventToRemove.outputFormatT)
          choices += Choice(1.0, diff => {clue.sentence.removeEvent(eventToRemove, false)(diff)})
        }
        // TODO implement or leave it here; do nothing
      }
      val proposals = for (choice: Choice <- choices.toList) yield
        choice.asProposalWithScoreAcceptance(model, obj, temperature)
      proposals
    }
  }

  class ThemesToEvents(val model: Model, obj: Model) extends ProposalSampler[Event] {
    def objective = obj
    temperature = InitFile.temperature

    def proposals(event: Event): Seq[cc.factorie.Proposal] = {
      val choices = new ArrayBuffer[Choice]

      // at a first shot, only when the event does not a theme already, which means it is a new one, otherwise, we do nothing
      if (!event.arguments.exists(a => a.roleType.value == "Theme")) {
        // over all events in the sentence
        // but on the same clue, where we want to generate a new event,
        // and it may not be used too often
        if (event.eventType.value.contains("egulation")) { // add events as theme only for regulations
          for (eventAsThemeCandidate: Event <- event.clue.sentence.events; if (eventAsThemeCandidate.clue != event.clue)) {
            if (eventAsThemeCandidate.isUsedAsArgument < InitFile.maxMultipleArgUseAsTheme && // do not use to often
                    (eventAsThemeCandidate.clue != event.clue) && // not a theme on the same span as the clue
                    //q(eventAsThemeCandidate != event) && // not myself
                    !(event.clue.events.exists((e: Event) => e.arguments.exists((a: Argument) => a.entity == eventAsThemeCandidate))) // do not add this as theme if another event is on the same clue and has this theme already
            ) {
              if (!(InitFile.semicolonAsBoundary && (Data.isSymbolBetweenTokens(";", event.clue.first, eventAsThemeCandidate.clue.first)))) {
                choices += Choice(1.0, diff => {
                  event.addEntityAsArgument(eventAsThemeCandidate, "Theme")(diff)
                  if (InitFile.doTouch && event.isATrueEventInSentence(true, true, false, null)) {
                    event.touchEventInTrueSentence(true, true, false)
                  }
                })
                if (!InitFile.allowStealingOfArguments) {

                }
              }
            } else {
              //System.err.println("Not using event "+eventAsThemeCandidate+" as it is already used "+eventAsThemeCandidate.isUsedAsArgument+" times as argument")
            }
          }
        }
        // over all genes in the sentence
        for (geneAsThemeCandidate: Gene <- event.clue.sentence.genes) {
          if (geneAsThemeCandidate.isUsedAsArgument < InitFile.maxMultipleArgUseAsTheme && // not to often
                  !(event.clue.events.exists((e: Event) => e.arguments.exists((a: Argument) => a.entity == geneAsThemeCandidate))) // do not add this as a theme of another event is on the same clue has this theme already
          ) {
            if (!(InitFile.semicolonAsBoundary && (Data.isSymbolBetweenTokens(";", event.clue.first, geneAsThemeCandidate.span.first)))) {
              choices += Choice(1.0, diff => {
                event.addEntityAsArgument(geneAsThemeCandidate, "Theme")(diff)
                if (InitFile.doTouch && event.isATrueEventInSentence(true, true, false, null))
                  event.touchEventInTrueSentence(true, true, false)
              })
            }
          } else {
            //System.err.println("Not using gene "+geneAsThemeCandidate+" as it is already used "+geneAsThemeCandidate.isUsedAsArgument+" times as argument")
          }
        }

        // if we have a theme already
      } else if (true) { // set false to only have events with themes
        // do nothing, only in the case there is a theme already
        choices += Choice(1.0, diff => {})

        // self regulation: if this is not a regulation event, we propose to add a regulation with this here as theme on the same clue
        if (InitFile.proposeSelfRegulation && (event.eventType.value == "Gene_expression")) {
          // check if the proposed event is there already
          if (!(event.clue.events.exists((other: Event) => other.eventType.value == "Positive_regulation" && other.arguments.exists(a => a.entity == event && a.roleType.value == "Theme")))) {
            choices += Choice(1.0, diff => {
              val newEvent = event.clue.sentence.addNewEvent(-1, -1, "Positive_regulation", event.clue)(diff)
              newEvent.addEntityAsArgument(event, "Theme")(diff)
              //System.err.println("Proposing "+newEvent.outputFormatE.trim+"    "+newEvent.outputFormatT.trim+"    "+event.outputFormatT.trim+" good: "+newEvent.isATrueEventInSentence(true,true,false,null))
            })
          }
        }

        // there is a theme already, let's add more themes (binding) or causes (regulations) or do nothing

        // regulations can take another gene or event as cause, only if there is non already!
        if (event.eventType.value.contains("egulation")) {
          for (span <- event.clue.sentence.spans; entityAsNewArgument: Entity <- span.entities;
               if (!event.arguments.exists(a => a == entityAsNewArgument) && // not adding the same again
                       entityAsNewArgument.isUsedAsArgument < InitFile.maxMultipleArgUseAsTheme && // do not use this gene as theme too often
                       entityAsNewArgument != event && // not adding itself as an argument
                       (!(entityAsNewArgument.isInstanceOf[Event] && entityAsNewArgument.asInstanceOf[Event].arguments.exists(a => a == event))) && // no cycle
                       (!event.arguments.exists(a => a.roleType.value == "Cause")) && // not two causes
                       (!(InitFile.semicolonAsBoundary && (Data.isSymbolBetweenTokens(";", event.clue.first, entityAsNewArgument.span.first)))) && // ; as boundary
                       (!event.clue.events.exists((other: Event) => other.deepEqual(true, true, false, event) && other.arguments.exists(a => a.entity == entityAsNewArgument && a.roleType.value == "Cause"))) && // do not produce duplicates
                       (!(InitFile.noSameStringArgs && (event.arguments.exists(a => a.entity.span.stringrepr == entityAsNewArgument.span.stringrepr)))) // no same string args
                       )) {
            choices += Choice(1.0, diff => {event.addEntityAsArgument(entityAsNewArgument, "Cause")(diff)})
          }
        }
        // bindings can take another gene as theme, no cause
        if (event.eventType.value == "Binding") { // all genes
          for (geneAsAnotherTheme: Gene <- event.clue.sentence.genes;
               if (event.arguments.length < 4 && // not more than three themes at binding
                       !event.arguments.exists(a => a.entity == geneAsAnotherTheme) && // not adding the same again
                       geneAsAnotherTheme.isUsedAsArgument < InitFile.maxMultipleArgUseAsTheme && // do not use this gene as theme too often
                       (!(InitFile.bindingHeuristic && forbiddenInBinding(event, geneAsAnotherTheme))) && //we only allow additional themes if there exist no theme on the same side of the clue
                       (!(InitFile.semicolonAsBoundary && (Data.isSymbolBetweenTokens(";", event.clue.first, geneAsAnotherTheme.span.first)))) && // ; as boundary
                       (!(event.clue.events.exists( // do not produce duplicates
                         (other: Event) => other.arguments.length - 1 == event.arguments.length && // only one is missing
                                 event.arguments.forall(a => other.arguments.exists(b => b.entity == a.entity)) && // all are in the other already
                                 other.arguments.exists(a => a.entity == geneAsAnotherTheme) // also the new one
                         ))) &&
                       (!(InitFile.noSameStringArgs && (event.arguments.exists(a => a.entity.span.stringrepr == geneAsAnotherTheme.span.stringrepr)))) // no same string args
                       )) {
            choices += Choice(1.0, diff => {event.addEntityAsArgument(geneAsAnotherTheme, "Theme")(diff)})
          }
        } else { // all others can do nothing more
          ;
        }

        // we could also remove arguments, as long as there are more than 1 and a theme remains
        if (event.arguments.length > 1) {
          for (argToRemove <- event.arguments; if (event.getThemes.filter(a => (a != argToRemove)).length > 0)) {
            //System.err.println("We could remove "+argToRemove+" from "+event+" which has args "+event.arguments.map(a=>a.roleType.value).mkString("~"))
            choices += Choice(1.0, diff => {event.removeEntityAsArgument(argToRemove)(diff)})
          }
        }

        //        // exchange arguments in myself (makes no sense for binding and other non-regulations) // very bad?
        if (event.eventType.value.contains("egulation") && (event.arguments.length < 1 || event.arguments.length > 2))
          throw new Exception("WHAT THE F...? -- Not fitting number of arguments in " + event.outputFormatE)
        if (InitFile.allowToExchangeTwoArgumentsInEvent && event.eventType.value.contains("egulation") && event.arguments.length == 2) { // they can only have two arguments, nothing else is possible
          // exchange entity between the arguments
          val a1 = event.arguments(0)
          val a2 = event.arguments(1)
          val t1 = a1.roleType.value
          val t2 = a2.roleType.value
          // remove and set newly
          choices += Choice(1.0, diff => {
            event.removeEntityAsArgument(a1)(diff)
            event.removeEntityAsArgument(a2)(diff)
            event.addEntityAsArgument(a1.entity, t2)(diff)
            event.addEntityAsArgument(a2.entity, t1)(diff)
          })
        }
        // propose to change the type
        // first, assume that only a theme is here, for the other cases, something else needs to be done
        if (event.arguments.length == 1 && InitFile.allowChangeOfRegulationType) {
          if (event.arguments.exists(a => a.entity.isInstanceOf[Event]) || event.arguments.exists(a => a.roleType.value == "Cause")) { // then we can only switch to another regulation
            if (event.eventType.value != "Positive_regulation") choices += Choice(1.0, diff => {event.changeEventTypeTo("Positive_regulation")(diff)})
            if (event.eventType.value != "Regulation") choices += Choice(1.0, diff => {event.changeEventTypeTo("Regulation")(diff)})
            if (event.eventType.value != "Negative_regulation") choices += Choice(1.0, diff => {event.changeEventTypeTo("Negative_regulation")(diff)})
          } else if (InitFile.allowChangeOfAllEventTypes) { // everything is possible, as we asume to only have one argument, hence, binding does not happen
            for (i <- 0 until 9; if (!(InitFile.ignoreregulations && (Data.possibleEventTypes(i).contains("egulation")))))
              if (event.eventType.value != Data.possibleEventTypes(i)) choices += Choice(1.0, diff => {event.changeEventTypeTo(Data.possibleEventTypes(i))(diff)})
          }
        } else if (event.getThemes.length > 1 && event.getThemes.length == event.arguments.length) { // binding with multiple themes, split them, no other args
          if (InitFile.allowSplittingOfBinding) {
            for (bindingtheme <- event.arguments) {
              for (i <- 0 until 9) {
                choices += Choice(1.0, diff => { // remove binding and propose a new single binding for each theme, splitting that binding to multiple ones removing the other entities
                  event.clue.sentence.removeEvent(event, false)(diff)
                  val newBinding = event.clue.sentence.addNewEvent(-1, -1, Data.possibleEventTypes(i), event.clue)(diff)
                  newBinding.addEntityAsArgument(bindingtheme.entity, "Theme")(diff)
                })
              }
            }
          }
        }
      }

      // allow to not add a theme and remove the whole event again, also allowed if a theme was already there
      choices += Choice(1.0, diff => {event.clue.sentence.removeEvent(event, false)(diff)})

      val proposals = for (choice: Choice <- choices.toList) yield
        choice.asProposalWithScoreAcceptance(model, obj, temperature)
      proposals
    }
  }

  def forbiddenInBinding(event: Event, geneAsAnotherTheme: Entity): Boolean = {
    event.arguments.exists((arg: Argument) => {
      (arg.entity.span.head.position < event.clue.mainClueToken.position) ==
              (geneAsAnotherTheme.span.head.position < event.clue.mainClueToken.position)
    })
  }

}
