package cc.refectorie.bionlp

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */


object ResultAugmenter {
  def main(args:Array[String]) : Unit = {
    val c = Data.readCorpus("data/corpora/augmenter/finalvalidation-exp50")
    augment(c)
  }

  def augment(c:Array[Document]) : Unit = {
    for(d:Document <- c) {
      val eventsInDoc = for(s <- d ; e:Event <- s.trueSentence.events) yield e
      // for each event, try to find similar events
      for(e:Event <- eventsInDoc) {
        if (!e.arguments.exists(a => a.entity.isInstanceOf[Event])) { // first, try only with gene arguments
          val clueString = e.clue.stringrepr
          for(a:Argument <- e.arguments) {
            val argString = a.entity.span.stringrepr
            // does the clueString-argString combination occur in another sentence, but not in that combination?
            for(otherSentence:Sentence <- d ; if (otherSentence.trueSentence != e.clue.sentence)) { // other sentences
              //otherSentence.trueSentence.spans.foreach(s => System.err.println(s.stringrepr+" =? "+clueString))
              val otherClueOpt = otherSentence.trueSentence.spans.find(s => s.stringrepr == clueString)
              val otherArgOpt = otherSentence.trueSentence.spans.find(s => s.stringrepr == argString)
              System.err.println(otherClueOpt+"----"+otherArgOpt)
              if (otherClueOpt.isDefined && !otherClueOpt.get.events.exists(e2 => e2.eventType.value == e.eventType.value)) {
                val newEvent = otherSentence.trueSentence.addNewEvent(-1,-1,e.eventType.value,otherClueOpt.get)(null)
                System.err.println("Adding "+newEvent.outputFormatE.trim+" "+newEvent.outputFormatT.trim)
                for(entityOnArgOpt:Entity <- otherArgOpt.get.entities) {
                  newEvent.addEntityAsArgument(entityOnArgOpt,"Theme")(null)
                  System.err.println("\t"+entityOnArgOpt.genericOutputFormatTorE)
                }
              }
            }
          }
        }
      }
      Data.writeCorpus(c,"data/corpora/augmenter/finalvalidation-exp50-augment",true)
    }
  }
}