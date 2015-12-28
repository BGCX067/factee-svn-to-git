package cc.refectorie.bionlp

import collection.mutable.HashMap
import io.Source
import cc.factorie._

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Authors: Sebastian Riedel, Roman Klinger
 * License: GPL2
 */


object DictionaryTemplates {
  class DictPrecision(eventType: String, precision: Double)
          extends RealVectorVariable(Map(eventType -> precision))

  class DictWithPrecisionTemplate(precFile: String) extends Template1[Event] with
          DotStatistics1[DictPrecision] {
    val precisions = new HashMap[String, Double]


    override def freezeDomains: Unit = {}

    def statistics(e: Event): Iterable[Stat] = {
      if (e.present) {
        val clueString = e.clue.map(_.word).mkString(" ")
        val precision = precisions.getOrElse(clueString, 0.0)
        Stat(new DictPrecision(e.eventType.value, precision))
      } else
        Nil
    }

    def loadFromFile(file: String) = {

      for (line <- Source.fromFile(file).getLines) {
        if (!line.startsWith(">")) {
          val split = line.split("\\s+")
          precisions(split(0).substring(1, split(0).length - 1)) = split(1).toDouble
        }
      }
      System.err.println("Loaded %d dictionary entries with precision from %s".format(precisions.size, file))
    }

    loadFromFile(precFile)


    def weightsToString: Iterable[String] = {
      val d1 = Domain[EventType]
      for (eventType <- Domain[EventType]) yield
        "%-35s %6f".format(eventType, weights dot new DictPrecision(eventType, 1.0).vector)
    }

  }

}