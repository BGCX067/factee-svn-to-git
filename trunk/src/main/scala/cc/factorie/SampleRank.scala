/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import cc.factorie.util.Implicits._
import scalala.tensor.Vector
import scalala.Scalala._
import cc.refectorie.bionlp.Templates.{EventClueArgumentTemplate}

/** Set the parameters so that the model.score ranks the top sample the same as the objective.score, with a margin. */
trait SampleRank extends ProposalSampler0 with SamplerOverSettings0 {
  this: ProposalSampler[_] =>
  type TemplatesToUpdate <: DotTemplate
  def model: Model
  var learningMargin = 1.0
  def updateWeights: Unit
  val amIMetropolis = this.isInstanceOf[MHSampler[Variable]] // TODO Can we find a way to avoid this special case?
  var logLevel = 0

  /** If objective has not yet been set non-null, get it from the Global.defaultObjective. */
  abstract override def objective = if (super.objective == null) Global.defaultObjective else super.objective

  var bestModel1, bestModel2, bestObjective1, bestObjective2, changeProposal : Proposal = null

  def predictedScore = changeProposal.modelScore
  def targetScore = changeProposal.objectiveScore

  abstract override def proposalsHook(proposals:Seq[Proposal]) : Unit = {
    if (proposals.length < 2) return
    super.proposalsHook(proposals)
    //println("SampleRank proposalsHook "+proposals.toList.map(_.toString))
    val bestModels = proposals.max2(_ modelScore)
    val bestObjectives = proposals.max2(_ objectiveScore)
    bestModel1 = bestModels._1
    bestModel2 = bestModels._2
    bestObjective1 = bestObjectives._1
    bestObjective2 = bestObjectives._2
    changeProposal = if(bestModel1.diff.size>0) bestModel1 else bestModel2
    assert(bestObjective1.objectiveScore == bestObjective1.objectiveScore) // Check not NaN
    assert(bestObjective2.objectiveScore == bestObjective2.objectiveScore)
    //val props = List(bestModel1, bestModel2, bestObjective1, bestObjective2)
    //println("SampleRank proposalsHook "+props.map(_.modelScore)+"  "+props.map(_.objectiveScore))

    if (logLevel > 0) {
      println("bestObjective1 "+bestObjective1)
      println("bestModel1     "+bestModel1)
      println("bestModel2     "+bestModel2)
      if (shouldUpdate) println("SHOULDUPDATE") else println("NOTUPDATE")
    }

    if (shouldUpdate) updateWeights
  }

  def shouldUpdate: Boolean = {
    if (amIMetropolis) {
      val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel2
      !(changeProposal.modelScore * changeProposal.objectiveScore > 0 || changeProposal.objectiveScore == 0)
    } else {
      // the objective function has some preference (e.g. we don't have an unlabeled example here)
      (bestObjective1.objectiveScore > bestObjective2.objectiveScore || bestObjective1.objectiveScore > bestModel1.objectiveScore) &&
      // the model got it wrong, or isn't confident enough about being right
      // TODO should this be based on acceptanceScore instead of modelScore?
      ((bestModel1 ne bestObjective1) || Math.abs(bestModel1.modelScore - bestModel2.modelScore) < learningMargin)
    }
  }

  def addGradient(accumulator:DotTemplate=>Vector, rate:Double): Unit = {

    /*
    List(bestModel1, bestModel2, bestObjective1, bestObjective2).foreach(p => println(p))
    println ("bestObjective1 objectiveScore = "+bestObjective1.objectiveScore)//+" value = "+bestTruth1.value)
    println ("bestObjective2 objectiveScore = "+bestObjective2.objectiveScore)//+" value = "+bestTruth1.value)
    println ("bestModel1     objectiveScore = "+bestModel1.objectiveScore)//+" value = "+bestScoring.value)
    println ("bestObjective1 modelScore = "+bestObjective1.modelScore)
    println ("bestObjective2 modelScore = "+bestObjective2.modelScore)
    println ("bestModel1     modelScore = "+bestModel1.modelScore)
    println ()
    */
    if (logLevel > 0) {
      println ("bestObjective1 ms="+bestObjective1.modelScore+" os="+bestObjective1.objectiveScore+" diff="+bestObjective1.diff)
      println ("bestObjective2 ms="+bestObjective2.modelScore+" os="+bestObjective2.objectiveScore+" diff="+bestObjective2.diff)
      println ("bestModel1     ms="+bestModel1.modelScore+" os="+bestModel1.objectiveScore+" diff="+bestModel1.diff)
      println ("bestModel2     ms="+bestModel2.modelScore+" os="+bestModel2.objectiveScore+" diff="+bestModel2.diff)
    }

    // Only do learning if the trueScore has a preference
    // It would not have a preference if the variable in question is unlabeled
    // TODO Is this the right way to test this though?  Could there be no preference at the top, but the model is selecting something else that is worse?
    if (shouldUpdate) {
      // If the model doesn't score the truth highest, then update parameters
      if (bestModel1 ne bestObjective1) { // TODO  I changed != to "ne"  OK?  Should I be comparing values here instead?
         if (logLevel < -1) System.err.println("Doing Update 1 in "+bestObjective1.diff.factorsOf[TemplatesToUpdate](model).size+" factors")
        for((factor:Factor) <- bestObjective1.diff.factorsOf[TemplatesToUpdate](model)) if (logLevel < -1) System.err.println("Template Class: "+factor.template.getClass+": "+(if (factor.template.isInstanceOf[EventClueArgumentTemplate]) factor.template.asInstanceOf[EventClueArgumentTemplate].weightStat else ""))
        // ...update parameters by adding sufficient stats of truth, and subtracting error
        //println ("SampleRank learning from error")
        //println (" Model #templates="+model.size)
        //println (" Updating bestObjective1 "+(bestObjective1.diff.factorsOf[WeightedLinearTemplate](model).size)+" factors")
        //println (" Updating bestModel1 "+(bestModel1.diff.factorsOf[WeightedLinearTemplate](model).size)+" factors")
        bestObjective1.diff.redo
        bestObjective1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector *  rate)
        for((factor:Factor) <- bestObjective1.diff.factorsOf[TemplatesToUpdate](model)) if (logLevel < -1) System.err.println("Template Class: "+factor.template.getClass+": "+(if (factor.template.isInstanceOf[EventClueArgumentTemplate]) factor.template.asInstanceOf[EventClueArgumentTemplate].weightStat else ""))
        bestObjective1.diff.undo
        bestObjective1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector * -rate)
        for((factor:Factor) <- bestObjective1.diff.factorsOf[TemplatesToUpdate](model)) if (logLevel < -1) System.err.println("Template Class: "+factor.template.getClass+": "+(if (factor.template.isInstanceOf[EventClueArgumentTemplate]) factor.template.asInstanceOf[EventClueArgumentTemplate].weightStat else ""))
        bestModel1.diff.redo
        bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector * -rate)
        for((factor:Factor) <- bestObjective1.diff.factorsOf[TemplatesToUpdate](model)) if (logLevel < -1) System.err.println("Template Class: "+factor.template.getClass+": "+(if (factor.template.isInstanceOf[EventClueArgumentTemplate]) factor.template.asInstanceOf[EventClueArgumentTemplate].weightStat else ""))
        bestModel1.diff.undo
        bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector *  rate)
        for((factor:Factor) <- bestObjective1.diff.factorsOf[TemplatesToUpdate](model)) if (logLevel < -1) System.err.println("Template Class: "+factor.template.getClass+": "+(if (factor.template.isInstanceOf[EventClueArgumentTemplate]) factor.template.asInstanceOf[EventClueArgumentTemplate].weightStat else ""))
      }
      else if (bestModel1.modelScore - bestModel2.modelScore < learningMargin) {
         if (logLevel < -1) System.err.println("Doing Update 2")
        // ...update parameters by adding sufficient stats of truth, and subtracting runner-up
        //println ("SampleRank learning from margin")
        // TODO Note This is changed from previous version, where it was bestTruth.  Think again about this.
        bestObjective1.diff.redo
        bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector *  rate)
        bestObjective1.diff.undo
        bestModel1.diff.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector * -rate)
        bestModel2.diff.redo
        bestModel2.diff.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector * -rate)
        bestModel2.diff.undo
        bestModel2.diff.factorsOf[TemplatesToUpdate](model).foreach(f => accumulator(f.template) += f.statistic.vector *  rate)
      }
    } //else Console.println ("No preference unlabeled "+variable)
  }
}

