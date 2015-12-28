package cc.refectorie.bionlp

import cc.factorie._
import collection.mutable.ArrayBuffer

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object IntelligentSampler {

  class MetaSampler(val model:Model, obj:Model, maxLengthOfClue:Int, theTemperature:Double,maxNumOfEventsOnOneSpan:Int,maxMultipleArgUseAsTheme:Int) extends ProposalSampler[Any] {
    val clueOnlySampler = new ClueOnlySampler(model,obj,maxLengthOfClue,theTemperature)
    val eventsWithThemeOverClues = new EventsWithThemeOverClues(model,obj,theTemperature,maxNumOfEventsOnOneSpan,maxMultipleArgUseAsTheme)
    def proposals(x:Any) : Seq[cc.factorie.Proposal] = {
      x match {
        case x:Token => clueOnlySampler.proposals(x)
        case x:Span => eventsWithThemeOverClues.proposals(x)
      }
    }
    def objective = obj
  }

  class ClueOnlySampler(val model:Model, obj:Model, maxLengthOfClue:Int, theTemperature:Double) extends ProposalSampler[Token] {

    if (false) {
      this.proposalsHooks += {
        (proposals:Seq[cc.factorie.Proposal]) => {
          proposals.foreach((x:cc.factorie.Proposal) => System.err.println("Diff="+x.diff+"; AccScore="+x.acceptanceScore+"; ModScore="+
                  x.modelScore+"; ObjScore="+x.objectiveScore)
            )
        }}
    }

    def proposals(token:cc.refectorie.bionlp.Token) : Seq[cc.factorie.Proposal]= {
      val temperature = theTemperature
      val sentence = token.sentence
      val tokenHasClue = sentence.spans.find((span:Span) => span.contains(token)) // this checks for gene and event spans, that is good!
      val choices = new ArrayBuffer[Choice]
      choices += Choice(1.0, diff => {})

      if (tokenHasClue.isDefined && tokenHasClue.get.isOfEvent) {
        //System.err.println("proposing to remove")
        choices += Choice(1.0, diff => {
          val span = tokenHasClue.get ;
          span.entities.foreach((e:Entity) => {
            if (e.isInstanceOf[Event]) {
              e.span.sentence.removeEvent(e.asInstanceOf[Event],false)(diff) // TODO check that
            }
          })
          sentence.removeSpan(span)(diff)
        })
      }
      if (!tokenHasClue.isDefined) {
        for(length <- 1 to maxLengthOfClue ; if (sentence.length > token.sentencePos+length-1) ; if (!(sentence.spans.exists((span:Span) => span.contains(sentence(token.sentencePos+length-1)) )))) { // sample different lengthes of the clue, mainly 1 will be true
          //System.err.println("proposing to add")
          choices += Choice(1.0, diff => {new Span(sentence,token.sentencePos,length,null,"T999")(diff) })
        }
      }
      
      val proposals = for (choice: Choice <- choices.toList) yield
        choice.asProposalWithScoreAcceptance(model, obj, temperature)
      proposals
    }
    def objective = obj
  }

  class EventsWithThemeOverClues(val model:Model, obj:Model, theTemperature:Double,maxNumOfEventsOnOneSpan:Int,maxMultipleArgUseAsTheme:Int) extends ProposalSampler[Span] {
    def objective = obj

    if (false) {
      this.proposalsHooks += {
        (proposals:Seq[cc.factorie.Proposal]) => {
          proposals.foreach((x:cc.factorie.Proposal) => System.err.println("Diff="+x.diff+"; AccScore="+x.acceptanceScore+"; ModScore="+
                  x.modelScore+"; ObjScore="+x.objectiveScore)
            )
        }}
    }

    // check if potential event is existing already, IF NOT, it is created and added to the difflist
    def makeEventAsChoiceIfNotAlreadyExisting(clue:Span,eventType:String,arguments:ArrayBuffer[(Entity,String)],caseNum:Int) : ArrayBuffer[Choice] = {
      val alsoMakeEventIfalreadyExisting = false
      // to return
      val choices = new ArrayBuffer[Choice]
      // is an event already existing on this span, only there!
      val equivalentEventExisting = clue.events.exists((e:Event) => {
          e.eventType.value == eventType &&
          e.arguments.length == arguments.length &&
          e.arguments.forall((oldarg:Argument) => arguments.exists(newarg => newarg._1 == oldarg.entity && newarg._2 == oldarg.roleType.value))
        })
      //if (clue.start == 12 && clue.length == 1 && eventType == "Positive_regulation" && clue.sentence.offsets._1 == 637 && arguments.exists(_._1.span.start==13) && arguments.exists(_._1.span.start==4))
      //  System.err.println("Propose? "+clue.start+";"+clue.length+":"+eventType+";"+arguments+" == "+(!equivalentEventExisting))
      if (alsoMakeEventIfalreadyExisting || !equivalentEventExisting) {
//              if (caseNum < 0) System.err.println(caseNum+", proposing "+clue+" with "+eventType+" with args "+arguments)
//        System.err.println("Proposing Event on "+clue+" as "+eventType+" with "+arguments)
        choices += new Choice(1.0, diff => {
          val newEvent = clue.sentence.addNewEvent(-1,-1,eventType,clue)(diff)
          for(i <- 0 until arguments.length) {
            newEvent.addEntityAsArgument(arguments(i)._1,arguments(i)._2)(diff)
          }
          // debug
          //if (newEvent.isATrueEventInSentence(true,true,null)) System.err.println(caseNum+", proposed a true event: "+newEvent.outputFormatE)
          val dotouch = InitFile.initKeyValues("showTrueButNeverProposed")
          if (dotouch != null && dotouch == "true" && newEvent.isATrueEventInSentence(true,false,true,null)) newEvent.touchEventInTrueSentence(true,false,true)
          // debug end
        })
      }
//      if (clue.first.word=="inhibit" && eventType == "Negative_regulation" && arguments.exists(_._1.span.first.word=="SOCS")) {
//        System.err.println("Sentence: "+clue.sentence.toString(true))
//        val p = for (choice: Choice <- choices.toList) yield choice.asProposalWithScoreAcceptance(model, obj, temperature)
//        p.foreach((x:cc.factorie.Proposal) => System.err.println("Diff="+x.diff+"; AccScore="+x.acceptanceScore+"; ModScore="+
//                  x.modelScore+"; ObjScore="+x.objectiveScore)
//            )
//      }
      choices
    }

    // in this method we check also if a gene is on the same span as the event, this is not necessary, as this method is only called on events.ofEvent
    def fillEmptySpanChoices(clue:Span, maxMultipleAsTheme:Int) : ArrayBuffer[Choice] = {
      val choices = new ArrayBuffer[Choice]
      val sentence = clue.sentence

      for(tn <- 0 to 8) {
        //choices += Choice(1.0, diff => {sentence.addNewEvent(-1,-1,Data.possibleEventTypes(tn),clue)(diff) })
        val a = Data.possibleEventTypes(tn)
        // can only take one gene as theme
        if (a=="Gene_expression"||a=="Transcription"||a=="Protein_catabolism"||a=="Localization"||a=="Phosphorylation"){
          // for each gene as argument, an event is proposed
          for(gene <- sentence.genes ; if (!gene.span.deepEqual(clue)) ; if (maxMultipleAsTheme==0 || gene.isUsedAsArgument < maxMultipleAsTheme)) {
            val args = new ArrayBuffer[(Entity,String)]
            args += (gene,"Theme")
            choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args,1)
          }
        }
        // can take two or more genes as themes
        if (a=="Binding") {
          // the documentation does not fit, but it also only be one! do that first
          for(gene1 <- sentence.genes ; if (!gene1.span.deepEqual(clue)) ; if (maxMultipleAsTheme==0 || gene1.isUsedAsArgument < maxMultipleAsTheme)) {
            val args = new ArrayBuffer[(Entity,String)]
            args += (gene1,"Theme")
            choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args,2)
          }
          // for each possible pair of genes, an event is proposed, the themes are symmetrical, so only one direction
          for(gene1 <- sentence.genes ; gene2 <- sentence.genes ; if (!gene1.span.deepEqual(clue) && !gene2.span.deepEqual(clue)) ; if (gene1.span.start < gene2.span.start) ; if (maxMultipleAsTheme==0 || (gene1.isUsedAsArgument<maxMultipleAsTheme && gene2.isUsedAsArgument<maxMultipleAsTheme))) { // TODO more genes are possble, implement somewhere, augmenting?
            val args = new ArrayBuffer[(Entity,String)]
            args += (gene1,"Theme")
            args += (gene2,"Theme")
            choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args,3)
          }
        }
        // can take one event or gene as theme or cause, cause is optional
        if (a=="Regulation"||a=="Positive_regulation"||a=="Negative_regulation"){
          // for each possible event and each possible gene, make events with only one of both and with both in both associations (theme,cause)
          // I think, only if the event is the theme, a gene can be added, but a gene can be alone as theme
          // also two genes can be in
          // event alone as theme
          for(event1 <- sentence.events ; if (!event1.clue.deepEqual(clue)) ; if (maxMultipleAsTheme==0 || event1.isUsedAsArgument<maxMultipleAsTheme)) {
            val args = new ArrayBuffer[(Entity,String)]
            args += (event1,"Theme")
            choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args,4)
          }
          // cause and theme are events
          for(event1 <- sentence.events ; event2 <- sentence.events; if (event1 != event2) && (!event1.clue.deepEqual(clue)) && (!event2.clue.deepEqual(clue))) {
            if (maxMultipleAsTheme==0 || event1.isUsedAsArgument<maxMultipleAsTheme) {
              val args = new ArrayBuffer[(Entity,String)]
              args += (event1,"Theme")
              args += (event2,"Cause")
              choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args,5)
            }
            if (maxMultipleAsTheme==0 || event2.isUsedAsArgument<maxMultipleAsTheme) {
              val args2 = new ArrayBuffer[(Entity,String)]
              args2 += (event2,"Theme")
              args2 += (event1,"Cause")
              choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args2,6)
            }
          }
          // gene as a cause
          for(event1 <- sentence.events ; gene <- sentence.genes ; if (!event1.clue.deepEqual(clue) && !gene.span.deepEqual(clue)) ;
              if (maxMultipleAsTheme==0 || (event1.isUsedAsArgument<maxMultipleAsTheme))) {
            val args = new ArrayBuffer[(Entity,String)]
            args += (event1,"Theme")
            args += (gene,"Cause")
            choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args,7)
          }
          // gene alone as theme
          for(gene <- sentence.genes ; if (!gene.span.deepEqual(clue)) ; if (maxMultipleAsTheme==0 || gene.isUsedAsArgument<maxMultipleAsTheme)) {
            val args = new ArrayBuffer[(Entity,String)]
            args += (gene,"Theme")
            choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args,8)
          }
          // genes, as cause and theme
          for(gene1 <- sentence.genes ; gene2 <- sentence.genes ; if (gene1 != gene2) && (!gene1.span.deepEqual(clue)) && (!gene2.span.deepEqual(clue))) {
            if (maxMultipleAsTheme==0 || (gene1.isUsedAsArgument<maxMultipleAsTheme)) {
              val args = new ArrayBuffer[(Entity,String)]
              args += (gene1,"Theme")
              args += (gene2,"Cause")
              choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args,9)
            }
            if (maxMultipleAsTheme==0 || (gene2.isUsedAsArgument<maxMultipleAsTheme)) {
              val args2 = new ArrayBuffer[(Entity,String)]
              args2 += (gene2,"Theme")
              args2 += (gene1,"Cause")
              choices ++= makeEventAsChoiceIfNotAlreadyExisting (clue,a,args2,10)
            }
          }
        }
      }
      choices
    }

    // takes this existing event and
    // - changes it by adding an argument which is not used until now TODO or should I allow others, too?
    // - copies it and exchanges exactly one argument by all possible others which are not used until now TODO or should I allow all?
    def changeExistingEventWithNewArguments(eventOnClue:Event) : ArrayBuffer[Choice] = {
      val choices = new ArrayBuffer[Choice]
      val sentence = eventOnClue.clue.sentence
      val allEventsOnSentence = sentence.events
      val a = eventOnClue.eventType.value
      // over entities which are not used as argument until now
      for((span:Span) <- sentence.spans ; (entity:Entity) <- span.entities ; if (!entity.isUsedAsArgumentBoolean)) {
      // over unused spans
      //for((span:Span) <- sentence.spans ; if (!allEventsOnSentence.exists(event => event.arguments.exists(arg => arg.entity.span == span)))) {
        if (entity.isInstanceOf[Gene]) {
          // can only take one gene as theme
          if (a=="Gene_expression"||a=="Transcription"||a=="Protein_catabolism"||a=="Localization"||a=="Phosphorylation"){
            // copy on same position and replace argument
            val args = new ArrayBuffer[(Entity,String)]
            args += (entity,"Theme")
            choices ++= makeEventAsChoiceIfNotAlreadyExisting(eventOnClue.clue,a,args,-10)
          }
          // can take two or more genes as themes
          if (a=="Binding") {
            // copy on same position and replace all arguments by the new one
            val args = new ArrayBuffer[(Entity,String)]
            args += (entity,"Theme")
            choices ++= makeEventAsChoiceIfNotAlreadyExisting(eventOnClue.clue,a,args,-11)
            // replace only one argument (for each) by the new one
            for(arg <- eventOnClue.arguments) {
              val args = new ArrayBuffer[(Entity,String)]
              for(arg2 <- eventOnClue.arguments ; if (arg2 != arg)) {
                args += (arg2.entity,"Theme")
              }
              choices ++= makeEventAsChoiceIfNotAlreadyExisting(eventOnClue.clue,a,args,-12)
            }
            // keep the event as it is but add the new argument
            choices += Choice(1.0, diff => {
              eventOnClue.addEntityAsArgument(entity,"Theme")(diff)
            })
            // if there is no cause, add one
            if (!eventOnClue.arguments.exists(a => a.roleType.value == "Cause")) {
              choices += Choice(1.0, diff => {
                eventOnClue.addEntityAsArgument(entity,"Cause")(diff)
              })
            }
          }
        }
        // can take one event or gene as theme or cause, cause is optional
        if (a=="Regulation"||a=="Positive_regulation"||a=="Negative_regulation"){
          // span can be event or gene!
          // make a copy and...
          // ... exchange the theme with the new one if there is no cause
          if (!eventOnClue.arguments.exists((a:Argument) => a.roleType.value=="Cause")) {
            choices += Choice(1.0, diff => {
              val newEvent = sentence.addNewEvent(-1,-1,a,eventOnClue.clue)(diff)
              eventOnClue.addEntityAsArgument(entity,"Theme")(diff)
            })
          }
          // ... exchange the cause by the new one
          choices += Choice(1.0, diff => {
            val newEvent = sentence.addNewEvent(-1,-1,a,eventOnClue.clue)(diff)
            val argOption = eventOnClue.arguments.find((a:Argument) => a.roleType.value=="Theme")
            if (argOption.isDefined) {
              eventOnClue.addEntityAsArgument(argOption.get.entity,"Theme")(diff)
              eventOnClue.addEntityAsArgument(entity,"Cause")(diff)
            }
          })
        }
      }
      choices
    }

    def proposals(clue:Span) : Seq[cc.factorie.Proposal]= {
      //if (clue.start == 2 && clue.start == 1) System.err.println("Proposing for "+clue) else System.err.println("-----------------")
      val temperature = theTemperature
      if (clue.isOfGene) throw new Exception("Do not sample over gene spans! I warn you!")
      val sentence = clue.sentence
      val eventsOnClue = clue.entities.filter(e => e.isInstanceOf[Event]).map(e => e.asInstanceOf[Event])
      val choices = new ArrayBuffer[Choice]
      choices += Choice(1.0, diff => {})
      if (!eventsOnClue.isEmpty) {
        for(eventOnClue <- eventsOnClue) {
          // remove existing event on that clue, but keep the span, of course
          choices += Choice(1.0, diff => { sentence.removeEvent(eventOnClue,false)(diff) })
          // copy move augment!
          if (InitFile.initKeyValues("allowChangeOfEvents")=="true") choices ++= changeExistingEventWithNewArguments(eventOnClue)
        }
        // allow to put multiple entities on one clue directly
        if (maxNumOfEventsOnOneSpan==0 || eventsOnClue.length < maxNumOfEventsOnOneSpan) {
          choices ++= fillEmptySpanChoices(clue,maxMultipleArgUseAsTheme)
        }
      }
      if (eventsOnClue.isEmpty) {
         // the span is still empty, let's do something intelligent
        choices ++= fillEmptySpanChoices(clue,maxMultipleArgUseAsTheme)
      }

      val proposals = for (choice: Choice <- choices.toList) yield
        choice.asProposalWithScoreAcceptance(model, obj, temperature)
      // DEBUG XXX
//      if (clue.first.word == "inhibit") {
//        proposals.foreach(x =>
//        System.err.println("Diff="+x.diff+"; AccScore="+x.acceptanceScore+"; ModScore="+
//                  x.modelScore+"; ObjScore="+x.objectiveScore))
//      }
      proposals
    }
  }
}
