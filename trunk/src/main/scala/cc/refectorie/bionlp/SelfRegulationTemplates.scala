package cc.refectorie.bionlp

import cc.factorie.{DotStatistics1, Template2, BinaryVectorVariable}

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Sebastian Riedel
 * License: GPL2
 */


object SelfRegulationTemplates {

  class SelfRegulationFeatureVector(reg:Event,event:Event) extends BinaryVectorVariable[String](Nil){
    this += "Type->Type: %s->%s".format(reg.eventType.value,event.eventType.value)
    this += "Type->Clue: %s->%s".format(reg.eventType.value,event.clue.stringrepr)
    this += "Clue->Type: %s->%s".format(reg.clue.stringrepr,event.eventType.value)
    this += "Clue: %s".format(reg.clue.stringrepr)
//    this.values.foreach(x => System.err.println(x))
//    System.err.println
  }

  class SelfRegulationTemplate extends Template2[Event,Event] with DotStatistics1[SelfRegulationFeatureVector]{
    override def freezeDomains = {} 
    def unroll2(reg:Event): Iterable[Factor] =
      for (arg:Argument <- reg.arguments;
           if (arg.entity.span == reg.clue &&
                   arg.entity.asInstanceOf[Event].eventType.value != reg.eventType.value)) yield
        Factor(reg,arg.entity.asInstanceOf[Event])

    def unroll1(event:Event): Iterable[Factor] =
      for (reg:Event <- event.span.sentence.events.filter(e => e.clue == event.clue &&
              e != event && e.eventType.value != event.eventType.value &&
              e.containsEntityAsArgumentNonDeep(event) )) yield
        Factor(reg,event)


    def statistics(reg:Event,event:Event): Iterable[Stat] = Stat(new SelfRegulationFeatureVector(reg,event))
  }

}