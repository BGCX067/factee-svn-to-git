package cc.refectorie.bionlp

import cc.factorie._
import collection.mutable.ArrayBuffer

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object Sampler {
  class NaiveEventOnlySampler(model:Model, obj:Model) extends MHSampler[Token](model) {
    override def objective = obj
    temperature = 0.01
    /* If there is an event on that token, delete that event, if there is non, build one with clue on that token */
    def propose(token:Token)(implicit d:DiffList) = {
      val sentence = token.sentence
      val tokenHasEvent = sentence.events.find((event:Event) => event.clue.contains(token))
      if (tokenHasEvent.isDefined) { // delete
        val event = tokenHasEvent.get
        sentence.removeEvent(event,true)(d)
      } else { // make a new clue and event
        val event = sentence.addNewRandomEvent(token.sentencePos)(d)
        ;
      }
      0.0
    }
  }

  class EventClueAndTypeSampler(val model:Model, obj:Model,sampleType:Boolean, maxLengthOfClue:Int,allowEventsOnSameSpanStart: Boolean,theTemperature:Double) extends ProposalSampler[Token] {
    def proposals(token:cc.refectorie.bionlp.Token) : Seq[cc.factorie.Proposal]= {
      val temperature = theTemperature
      val sentence = token.sentence
      val tokenHasEvent = sentence.events.find((event:Event) => event.clue.contains(token))
      val choices = new ArrayBuffer[Choice]
      choices += Choice(1.0, diff => {})
      if (tokenHasEvent.isDefined) {
        choices += Choice(1.0, diff => { val event = tokenHasEvent.get ; sentence.removeEvent(event,true)(diff) })
      }
      if (allowEventsOnSameSpanStart || !tokenHasEvent.isDefined) {
        for(length <- 1 to maxLengthOfClue ; if (sentence.length > token.sentencePos+length+1)) { // sample different lengthes of the clue, mainly 1 will be true
          if (!(sentence.genes.exists(g => g.span.start == token.sentencePos && g.span.length == length ))) { // only if there is no gene on that position
            if (sampleType) {
              for(tn <- 0 to 8) {
                choices += Choice(1.0, diff => {sentence.addNewEvent(token.sentencePos,length,Data.possibleEventTypes(tn),null)(diff)})
              }
            } else {
              choices += Choice(1.0, diff => {sentence.addNewEvent(token.sentencePos,length,Data.possibleEventTypes(9),null)(diff)})
            }
          }
        }
      }
      val proposals = for (choice: Choice <- choices.toList) yield
        choice.asProposalWithScoreAcceptance(model, obj, temperature)
      proposals
    }

    def objective = obj
  }

  class ArgumentToEventSampler(val model:Model, obj:Model,theTemperature:Double) extends ProposalSampler[Event] {
    def objective = obj
    this.proposalsHooks += {
      (proposals:Seq[cc.factorie.Proposal]) => {
        proposals.foreach((x:cc.factorie.Proposal) => if (Workflow.debug) Workflow.deberr("Diff="+x.diff+"; AccScore="+x.acceptanceScore+/*"; bfRatio=N/A"+()+*/"; ModScore="+
                x.modelScore+"; ObjScore="+x.objectiveScore/*+"; Temp=N/A"+()*/)
          )
        //  System.err.println(proposals.mkString("\n"))
      }}

    override def proposalHook(proposal:Proposal) {
      //Workflow.deberr(Workflow.tmpdebug, "Next round with "+proposal.objectiveScore)
      if (proposal.modelScore != 0.0) if (Workflow.debug) Workflow.deberr("ModScore 1: "+proposal.modelScore)
      super.proposalHook(proposal)
    }

    // proposes to add each gene and event to the given event
    def proposals(event:Event) : Seq[cc.factorie.Proposal]= {
      temperature = theTemperature
      val choices = new ArrayBuffer[Choice]
      choices += Choice(1.0, diff => {})
      // propose to add genes and events as arguments
      Workflow.deberr("Proposing adding "+((event.clue.sentence.events++event.clue.sentence.genes).length)+" = "+event.clue.sentence.events.length+" + "+event.clue.sentence.genes.length)
      for(entityAsArgument <- event.clue.sentence.events++event.clue.sentence.genes ) { // propose each event and gene as argument
        for(argType <- 0 to 1) {
          choices += Choice(1.0, diff => { // adds entities as argument, for each possible event and gene in sentence
            if (
            // if such entity is already in, we do not propose it
              !event.arguments.find((arg:Argument) => arg.entity.deepEqual(entityAsArgument)).isDefined &&
                      // do not add myself
                      entityAsArgument != event &&
                      // only regulations can have events as arguments, filter that
                      (entityAsArgument.isInstanceOf[Gene] || event.eventType.value.equals("Regulation") || event.eventType.value.equals("Positive_regulation") || event.eventType.value.equals("Negative_regulation") )
            ) {
              val arg = event.addEntityAsArgument(entityAsArgument,Data.possibleArgTypes(argType))(diff)
              if (Workflow.debug) Workflow.deberr("Adding ("+(arg.isATrueArgumentInEvent(event,true))+")"+entityAsArgument.span.map(_.word).mkString(" ")+" to "+event.clue.map(_.word).mkString(" "))
              ;
            }
          })
        }
      }
      // propose to remove each existing argument in this event
      Workflow.deberr("Proposing removing ")
      for((argToBeRemoved:Argument) <- event.arguments) {
        choices += Choice(1.0, diff => {
          if (Workflow.debug) Workflow.deberr("Removing ("+(argToBeRemoved.isATrueArgumentInEvent(event,true))+")"+argToBeRemoved.entity.span.map(_.word).mkString(" ")+" from "+event.clue.map(_.word).mkString(" "))
          val arg = event.removeEntityAsArgument(argToBeRemoved)(diff)
          ;
        })
      }

      val proposals = for (choice: Choice <- choices.toList) yield choice.asProposalWithScoreAcceptance(model, obj, temperature)
      //proposals.foreach(x => Workflow.deberr(Workflow.tmpdebug,"PropMS>>>"+x.modelScore))
      proposals
    }
  }
}