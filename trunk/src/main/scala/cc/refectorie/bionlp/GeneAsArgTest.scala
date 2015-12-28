package cc.refectorie.bionlp

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object GeneAsArgTest {
    def main(args:Array[String]) : Unit = {
      var trainDocs1 = Data.readCorpus("data/corpora/development")

      for(doc <- trainDocs1 ; s <- doc ; g:Gene <- s.trueSentence.genes) {

        val m2 = g.span.first.sentencePos-2
        val m1 = g.span.first.sentencePos-1
        val p1 = g.span.last.sentencePos+1
        val p2 = g.span.last.sentencePos+2

        val em2 = if (m2 > -0) s.trueSentence.events.exists(e=>e.arguments.exists(a=>a.entity.span.contains(s.trueSentence(m2)))) else false
        val em1 = if (m1 > -1) s.trueSentence.events.exists(e=>e.arguments.exists(a=>a.entity.span.contains(s.trueSentence(m1)))) else false
        val eg = s.trueSentence.events.find(e=>e.arguments.exists(a=>a.entity==g))
        val ep1 = if (p1 < s.length-1) s.trueSentence.events.exists(e=>e.arguments.exists(a=>a.entity.span.contains(s.trueSentence(p1)))) else false
        val ep2 = if (p2 < s.length-2) s.trueSentence.events.exists(e=>e.arguments.exists(a=>a.entity.span.contains(s.trueSentence(p2)))) else false

        val gm2 = if (m2 > -0) s.genes.exists(g => g.span.contains(s(m2))) else false
        val gm1 = if (m1 > -1) s.genes.exists(g => g.span.contains(s(m1))) else false
        val gg = (g.span.isOfGene)
        val gp1 = if (p1 < s.length-1) s.genes.exists(g => g.span.contains(s(p1))) else false
        val gp2 = if (p2 < s.length-2) s.genes.exists(g => g.span.contains(s(p2))) else false

        if (gm2 || gm1 || gp1 || gp2) {
          System.err.println(s.toString(true))
          if (m2 > -0) System.err.println("   "+s(m2).word+"\t"+gm2+"\t"+em2)
          if (m1 > -1) System.err.println("   "+s(m1).word+"\t"+gm1+"\t"+em1)
          System.err.println("G: "+g.span.stringrepr+"\t"+gg+"\t"+eg.map(_.outputFormatE))
          if (p1 < s.length-1) System.err.println("   "+s(p1).word+"\t"+gp1+"\t"+ep1)
          if (p2 < s.length-2) System.err.println("   "+s(p2).word+"\t"+gp2+"\t"+ep2)

          System.err.println
        }
      }
      System.exit(1)
    }
}
