package cc.refectorie.bionlp

import scala.io.Source
import cc.factorie._
import scala.collection.mutable._
import edu.stanford.nlp.trees.MemoryTreebank
import edu.stanford.nlp.trees.TreeNormalizer
import edu.stanford.nlp.trees.PennTreeReader
import edu.stanford.nlp.trees.LabeledScoredTreeFactory
import edu.stanford.nlp.trees.TreeBankGrammaticalStructureWrapper
import edu.stanford.nlp.trees.GrammaticalStructure
import edu.stanford.nlp.trees.EnglishGrammaticalStructure
import java.io.StringReader

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object POSReader {
	def attachToDoc(ptreeFilename:String,doc:Document) {
		// output?
				val printCurrentSentence = false
				val debug = false
				// while running through the lines, we run through all sentences in document
				var currentSentenceId = 0
				for (ptreeLine <- Source.fromFile(ptreeFilename).getLines; if (currentSentenceId<doc.size)) {
					// the lines should be identical, but for the problematic case, we have our own counter, let's see if we need it
					val currentSentence = doc(currentSentenceId)
					if (printCurrentSentence) System.err.println(currentSentence)
					// each line in the file contains a whole parse tree for the sentence
					if (debug) System.err.println(ptreeLine)
					val tb = new MemoryTreebank(new TreeNormalizer())
					val tr = new PennTreeReader(new StringReader(ptreeLine), new LabeledScoredTreeFactory());
					val tree = tr.readTree()
					val leaves = tree.getLeaves
					
					if (leaves.size != currentSentence.length) {
						throw new Exception("Ptree and Sentence do not fit! "+currentSentence)
					}
     
					for(leaveIndex <- 0 to leaves.size-1) {
					  val leave = leaves.get(leaveIndex)
					  val pos = leave.parent(tree).label.value
					  val tokentext = leave.label
					  if (debug) System.err.println(tokentext+" "+currentSentence(leaveIndex).word+" "+pos)
					  // map that to tokens in sentence
					  currentSentence(leaveIndex) += pos
            currentSentence(leaveIndex).pos = pos
					}
          //currentSentence.foreach(x => System.err.print(x.word+"|"+x.pos+" ")) ; System.err.println
					currentSentenceId += 1
				}
	}
}

