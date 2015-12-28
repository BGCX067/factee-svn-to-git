/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.collection.mutable.{HashMap}
import cc.factorie.util.{Hooks0}

/** Samplers that key off of particular contexts.  Subclasses implement "process1(context:C)" */


// So we can call super in traits that override these methods
// TODO Is there a better way to do this?
// TODO Remove this and put ProposalSampler instead?  But then problems with SampleRank trait re-overriding methods it shouldn't?  Look into this. 


/** Samplers that generate a list of Proposal objects, and select one log-proportionally to their modelScore. */


// Not intended for users.  Here just so that SampleRank can override it.
// TODO is there a better way to do this?


/** Tries each one of the settings in the Iterator provided by the abstract method "settings(C), 
    scores each, builds a distribution from the scores, and samples from it. */


/** Manage and use a queue to more often revisit low-scoring factors and re-sample their variables. */
// Consider FactorQueue { this: Sampler[_] => ... abstract override postProcessHook(C,DiffList).  But what to do about C?  


// TODO But I haven't been able to make this existential typing work in practice yet.
/*
trait AlternativeFactorQueue extends Sampler[C forSome {type C <: Variable}] {
  override def diffHook(diff:DiffList): Unit = {
   println("foo") 
  }
}
*/
trait Sampler[C] {
  type ContextType = C
  //if (contextClass == classOf[Nothing]) throw new Error("Constructor for class "+this.getClass.getName+" must be given type argument");
  /** The number of calls to process(numIterations:Int) or process(contexts:C,numIterations:Int). */
  var iterationCount = 0
  /** The number of calls to process(context:C) */
  var processCount = 0
  /** The number of calls to process that resulted in a change (a non-empty DiffList) */
  var changeCount = 0
  /** Do one step of sampling.  This is a method intended to be called by users.  It manages hooks and processCount. */
  final def process(context:C): DiffList = {
    //System.err.println("1")
    val c = preProcessHook(context) // rk: returns the context put into
    if (c == null && !processingWithoutContext) return null // TODO should we return newDiffList here instead?
    val d = process1(c) // rk: process returns a diff list
    processCount += 1
    postProcessHook(c, d)
    diffHook(d)
    if (d != null && d.size > 0) changeCount += 1
    //System.err.println("8")
    d
  }
  /** If true, calls to "newDiffList" will create a new DiffList to describe the changes they made, otherwise "newDiffList" will return null. */
  var makeNewDiffList = true
  /** Convenient method for setting makeNewDiffList to false, and returning this. */
  def noDiffList: this.type = { makeNewDiffList = false; this }
  /** In your implementation of "process1" use this method to optionally create a new DiffList, obeying "makeNewDiffList". */
  def newDiffList = if (makeNewDiffList) new DiffList else null
  /** The underlying protected method that actually does the work.  Use this.newDiffList to optionally create returned DiffList.
      Needs to be defined in subclasses. */
  def process1(context:C): DiffList // TODO Why isn't this 'protected'?  It should be.  -akm.
  protected final def processN(contexts:Iterable[C]): Unit = {
    contexts.foreach(process(_))
    iterationCount += 1
    postIterationHooks
    if (!postIterationHook) return
  }
  def process(contexts:Iterable[C], numIterations:Int): Unit = for (i <- 0 to numIterations) processN(contexts)
  private var processingWithoutContext = false
  def process(count:Int): Unit = {
    processingWithoutContext = true // examined in process()
    for (i <- 0 to count) process(null.asInstanceOf[C]) // TODO Why is this cast necessary?;
    processingWithoutContext = false
  }

  // Hooks
  /** Called just before each step of sampling.  Return an alternative variable if you want that one sampled instead.
      Return null if you want to abort sampling of this context. */
  def preProcessHook(context:C): C = context
  /** Call just after each step of sampling. */
  def postProcessHook(context:C, difflist:DiffList): Unit = {}
  /** An alternative to postProcessHook that does not require the type C. */ // TODO Really have both?  Remove 'context:C' from postProcessHook?
  def diffHook(difflist:DiffList): Unit = {}
  /** Called after each iteration of sampling the full list of variables.  Return false if you want sampling to stop early. */
  def postIterationHook: Boolean = true
  def postIterationHooks = new Hooks0
}