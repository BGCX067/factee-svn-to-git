package cc.refectorie.bionlp

import collection.mutable.{HashMap, HashSet, ArrayBuffer}
import java.text.DecimalFormat
import util.Sorting

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object DocWideStats {
  def main(args:Array[String]) : Unit = {
    part2
  }

  def part2 : Unit = {
    var trainDocs1 = Data.readCorpus("data/corpora/development")
    val df = new DecimalFormat("###0.000")

    // clue transitions
//    var count = new HashMap[String,Int]
//    var normCount = new HashMap[String,Int]
//    for(d <- trainDocs1) {
//      for(s <- d) {
//        for(e:Event <- s.trueSentence.events) {
//          val nextSpans:Seq[Span] = MCEventTemplates.nextSpansWithSameString(e.clue,true)
//          if (!nextSpans.isEmpty) {
//            for(ne:Event <- nextSpans.first.events) {
//              val --> = e.eventType.value+"-->"+ne.eventType.value
//              if (!count.contains(-->)) count += --> -> 0
//              count += --> -> (count(-->)+1)
//
//              if (!normCount.contains(e.eventType.value)) normCount += e.eventType.value -> 0
//              normCount += e.eventType.value -> (normCount(e.eventType.value)+1)
//            }
//            if (nextSpans.first.events.isEmpty) {
//              val --> = e.eventType.value+"-->"+"NOEVENT"
//              if (!count.contains(-->)) count += --> -> 0
//              count += --> -> (count(-->)+1)
//
//              if (!normCount.contains(e.eventType.value)) normCount += e.eventType.value -> 0
//              normCount += e.eventType.value -> (normCount(e.eventType.value)+1)
//            }
//          }
//        }
//      }
//    }
//    for((f : String) <- count.keySet) {
//      val fromType = f.substring(0,f.indexOf("-->"))
//      val norm = normCount(fromType).toDouble
//      val normalized = count(f).toDouble / norm
//      System.err.println(f +"\t"+df.format(normalized)+" ("+count(f)+"/"+norm+")")
//    }
//
//    System.err.println
//



    // clue transitions with first sentence has very important like feature
//    count = new HashMap[String,Int]
//    normCount = new HashMap[String,Int]
//    for(d <- trainDocs1) {
//      for(s <- d) {
//        for(e:Event <- s.trueSentence.events) {
//          val nextSpans:Seq[Span] = MCEventTemplates.nextSpansWithSameString(e.clue,true)
//          if (!nextSpans.isEmpty) {
//            for(ne:Event <- nextSpans.first.events) {
//              val --> = e.eventType.value+"-->"+ne.eventType.value+"::"+firstSentenceHasEvent(d,e.clue.stringrepr)
//              if (!count.contains(-->)) count += --> -> 0
//              count += --> -> (count(-->)+1)
//
//              if (!normCount.contains(e.eventType.value)) normCount += e.eventType.value -> 0
//              normCount += e.eventType.value -> (normCount(e.eventType.value)+1)
//            }
//            if (nextSpans.first.events.isEmpty) {
//              val --> = e.eventType.value+"-->"+"NOEVENT"+firstSentenceHasEvent(d,e.clue.stringrepr)
//              if (!count.contains(-->)) count += --> -> 0
//              count += --> -> (count(-->)+1)
//
//              if (!normCount.contains(e.eventType.value)) normCount += e.eventType.value -> 0
//              normCount += e.eventType.value -> (normCount(e.eventType.value)+1)
//            }
//          }
//        }
//      }
//    }
//    for((f : String) <- count.keySet) {
//      val fromType = f.substring(0,f.indexOf("-->"))
//      val norm = normCount(fromType).toDouble
//      val normalized = count(f).toDouble / norm
//      System.err.println(f +"\t"+df.format(normalized)+" ("+count(f)+"/"+norm+")")
//    }
//
//    System.err.println



    // gene arg transitions
    var count = new HashMap[String,Int]
    var normCount = new HashMap[String,Int]
    for(d <- trainDocs1) {
      for(s <- d) {
        for(e <- s.trueSentence.events) {
          for(a1:Argument <- e.arguments) {
            val argCandidateSpans = MCEventTemplates.nextSpansWithSameString(a1.entity.span,true)

            val firstRelevantSpan = argCandidateSpans.find(s => s.sentence.events.exists(e=>e.arguments.exists(a=>a.entity.span==s)))
            // as which argument object?
            if (firstRelevantSpan.isDefined) {
              val firstArgs = for(e <- firstRelevantSpan.get.sentence.events ; a <- e.arguments ; if (a.entity.span == firstRelevantSpan.get)) yield a
              for(a:Argument <- firstArgs) {
                val trans = a1.eventWithThisArgument.eventType.value+"-->"+a.eventWithThisArgument.eventType.value
                if (trans == "Transcription-->Positive_regulation") {
                  System.err.println(a1)
                  System.err.println(a)
                  System.err.println(a1.entity.span.sentence)
                  System.err.println(a.entity.span.sentence)
                  System.err.println(a.entity.span.sentence.doc.text)
                }
                
                
                if (!count.contains(trans)) count += trans -> 0
                count += trans -> (count(trans)+1)
                if (!normCount.contains(a1.eventWithThisArgument.eventType.value)) normCount += a1.eventWithThisArgument.eventType.value -> 0
                normCount += a1.eventWithThisArgument.eventType.value -> (normCount(a1.eventWithThisArgument.eventType.value) + 1)
              }
            } else { // comment in for none
//              val trans = a1.eventWithThisArgument.eventType.value+"-->"+"NONE"
//              if (!count.contains(trans)) count += trans -> 0
//              count += trans -> (count(trans)+1)
//              if (!normCount.contains(a1.eventWithThisArgument.eventType.value)) normCount += a1.eventWithThisArgument.eventType.value -> 0
//              normCount += a1.eventWithThisArgument.eventType.value -> (normCount(a1.eventWithThisArgument.eventType.value) + 1)
            }



            // backwards
//            val argCandidateSpans2 = MCEventTemplates.previousSpansWithSameString(a1.entity.span,true)
//
//            val firstRelevantSpan2 = argCandidateSpans2.find(s => s.sentence.events.exists(e=>e.arguments.exists(a=>a.entity.span==s)))
//            // as which argument object?
//            if (firstRelevantSpan2.isDefined) {
//              val firstArgs = for(e <- firstRelevantSpan2.get.sentence.events ; a <- e.arguments ; if (a.entity.span == firstRelevantSpan2.get)) yield a
//              for(a:Argument <- firstArgs) {
//                val trans = a.eventWithThisArgument.eventType.value+"-->"+a1.eventWithThisArgument.eventType.value
//                if (!count.contains(trans)) count += trans -> 0
//                count += trans -> (count(trans)+1)
//                if (!normCount.contains(a1.eventWithThisArgument.eventType.value)) normCount += a1.eventWithThisArgument.eventType.value -> 0
//                normCount += a1.eventWithThisArgument.eventType.value -> (normCount(a1.eventWithThisArgument.eventType.value) + 1)
//              }
//            } else { // comment in for none
//              val trans = "NONE"+"-->"+a1.eventWithThisArgument.eventType.value
//              if (!count.contains(trans)) count += trans -> 0
//              count += trans -> (count(trans)+1)
//              if (!normCount.contains("NONE")) normCount += "NONE" -> 0
//              normCount += "NONE" -> (normCount("NONE") + 1)
//            }
//


          }
        }
      }
    }

    val tosortkeys = new ArrayBuffer[String]
    tosortkeys ++= count.keySet


    for((f : String) <- Sorting.stableSort(tosortkeys)) {
      val fromType = f.substring(0,f.indexOf("-->"))
      val norm = normCount(fromType).toDouble
      val normalized = count(f).toDouble / norm
      System.err.println(f +"\t"+df.format(normalized)+" ("+count(f)+"/"+norm+")")
    }

    System.err.println()


    //System.exit(0)






    // gene arg transitions with check of that gene is somewhat important
    InitFile.miGeneFrequencyEvents = true

    count = new HashMap[String,Int]
    normCount = new HashMap[String,Int]
    for(d:Document <- trainDocs1) {
      for(s <- d) {
        for(e <- s.trueSentence.events) {
          for(a1:Argument <- e.arguments) {
            val argCandidateSpans = MCEventTemplates.nextSpansWithSameString(a1.entity.span,true)

            val firstRelevantSpan = argCandidateSpans.find(s => s.sentence.events.exists(e=>e.arguments.exists(a=>a.entity.span==s)))
            // as which argument object?
            if (firstRelevantSpan.isDefined) {
              val firstArgs = for(e <- firstRelevantSpan.get.sentence.events ; a <- e.arguments ; if (a.entity.span == firstRelevantSpan.get)) yield a
              for(a:Argument <- firstArgs) {
                val trans = a1.eventWithThisArgument.eventType.value+"-->"+a.eventWithThisArgument.eventType.value+"::"+(a1.entity.span.stringrepr==d.mostImportantGene)
                if (!count.contains(trans)) count += trans -> 0
                count += trans -> (count(trans)+1)
                if (!normCount.contains(a1.eventWithThisArgument.eventType.value)) normCount += a1.eventWithThisArgument.eventType.value -> 0
                normCount += a1.eventWithThisArgument.eventType.value -> (normCount(a1.eventWithThisArgument.eventType.value) + 1)
              }
            } else {
              val trans = a1.eventWithThisArgument.eventType.value+"-->"+"NONE"+"::"+(a1.entity.span.stringrepr==d.mostImportantGene)
              if (!count.contains(trans)) count += trans -> 0
              count += trans -> (count(trans)+1)
              if (!normCount.contains(a1.eventWithThisArgument.eventType.value)) normCount += a1.eventWithThisArgument.eventType.value -> 0
              normCount += a1.eventWithThisArgument.eventType.value -> (normCount(a1.eventWithThisArgument.eventType.value) + 1)
            }
          }
        }
      }
    }
    for((f : String) <- count.keySet) {
      val fromType = f.substring(0,f.indexOf("-->"))
      val norm = normCount(fromType).toDouble
      val normalized = count(f).toDouble / norm
      System.err.println(f +"\t"+df.format(normalized)+" ("+count(f)+"/"+norm+")")
    }

    System.err.println()

    //System.exit(1)










    // gene arg transitions, recursive
    count = new HashMap[String,Int]
    normCount = new HashMap[String,Int]
    for(d <- trainDocs1) {
      for(s <- d) {
        for(e <- s.trueSentence.events) {
          for(a1:Argument <- e.arguments) {
            val argCandidateSpans = MCEventTemplates.nextSpansWithSameString(a1.entity.span,true)
            val candidate = argCandidateSpans.find(s => {
              s.sentence.events.exists(e1 => {
                e1.eventType.value == a1.eventWithThisArgument.eventType.value && // same eventtype
                        e1.arguments.exists(a => {
                          a.entity.isInstanceOf[Event] &&
                                  a.entity.asInstanceOf[Event].arguments.exists(aa=>{aa.entity.span==s})}) // has arg which is event and has span as arg
              })
            })
            if (candidate.isDefined) {
              val e2e3 =
              for(eLevel2 <- candidate.get.sentence.events ; eLevel1 <- eLevel2.arguments.filter(x=>x.entity.isInstanceOf[Event]) ;
                  if (eLevel1.entity.asInstanceOf[Event].arguments.exists(x=>x.entity.span==candidate.get) && eLevel2.eventType.value==a1.eventWithThisArgument.eventType.value))
              yield (eLevel1.entity.asInstanceOf[Event],eLevel2)
              for((e2,e3) <- e2e3) {
                val trans = a1.eventWithThisArgument.eventType.value+"-->"+e2.eventType.value
                if (!count.contains(trans)) count += trans -> 0
                count += trans -> (count(trans)+1)
                if (!normCount.contains(a1.eventWithThisArgument.eventType.value)) normCount += a1.eventWithThisArgument.eventType.value -> 0
                normCount += a1.eventWithThisArgument.eventType.value -> (normCount(a1.eventWithThisArgument.eventType.value) + 1)

              }
            } else {
              val trans = a1.eventWithThisArgument.eventType.value+"-->"+"NONE"
              if (!count.contains(trans)) count += trans -> 0
              count += trans -> (count(trans)+1)
              if (!normCount.contains(a1.eventWithThisArgument.eventType.value)) normCount += a1.eventWithThisArgument.eventType.value -> 0
              normCount += a1.eventWithThisArgument.eventType.value -> (normCount(a1.eventWithThisArgument.eventType.value) + 1)

            }
          }
        }
      }
    }
    for((f : String) <- count.keySet) {
      val fromType = f.substring(0,f.indexOf("-->"))
      val norm = normCount(fromType).toDouble
      val normalized = count(f).toDouble / norm
      System.err.println(f +"\t"+df.format(normalized)+" ("+count(f)+"/"+norm+")")
    }

  }

  def part1 : Unit = {
    var trainDocs1 = Data.readCorpus("data/corpora/development")
    var allEvents = for(d<-trainDocs1;s<-d;e<-s.trueSentence.events) yield e
    var allSpans = for(d <- trainDocs1 ; s <- d ; span <- s.trueSentence.spans) yield span
    var d1 = new HashSet[Document]

    System.err.print("Clue in different sentences: ")
    var c = 0
    var n = 0
    for(e:Event <- allEvents) {
      if (allEvents.exists(o => {
        e.span.sentence.doc == o.span.sentence.doc &&
                o.clue.sentence != e.clue.sentence &&
                e.eventType.value == o.eventType.value &&
                e.clue.stringrepr == o.clue.stringrepr
      })) {
        //d1.addEntry(e.clue.sentence.doc)
        c += 1
      } else
        n += 1
    }
    System.err.println(c+"/"+n)

    System.err.print("Clue in different sentences without recognizing the type: ")
    c = 0
    n = 0
    for(e:Event <- allEvents) {
      if (allEvents.exists(o => {
        e.span.sentence.doc == o.span.sentence.doc &&
                o.clue.sentence != e.clue.sentence &&
                e.clue.stringrepr == o.clue.stringrepr
      })) {
        //d1.addEntry(e.clue.sentence.doc)
        val ot = allEvents.find(o => {
          o.clue.sentence != e.clue.sentence &&
                  e.clue.stringrepr == o.clue.stringrepr
        }).get
        //System.err.println(e.eventType.value+(if (e.eventType.value == ot.eventType.value) " == " else " != ")+ot.eventType.value)
        //if (e.eventType.value != ot.eventType.value) System.err.println(e.eventType.value +" != "+ ot.eventType.value)
        c += 1
      } else
        n += 1
    }
    System.err.println(c+"/"+n)

    System.err.print("Clue in different sentences, recognizing the type, but all regulations together ")
    c = 0
    n = 0
    for(e:Event <- allEvents) {
      if (allEvents.exists(o => {
        e.span.sentence.doc == o.span.sentence.doc &&
                o.clue.sentence != e.clue.sentence &&
                e.clue.stringrepr == o.clue.stringrepr &&
                (e.eventType.value.contains("egulation") == o.eventType.value.contains("egulation") || e.eventType.value == o.eventType.value)
      })) {
        //d1.addEntry(e.clue.sentence.doc)
        c += 1
      } else
        n += 1
    }
    System.err.println(c+"/"+n)

    System.err.print("Event Clue String existent in other sentences, but not as clue:")
    c = 0
    n = 0
    for(e <- allEvents) {
      if (!allSpans.exists((s:Span) => {
        e.span.sentence.doc == s.sentence.doc &&
                s.sentence != e.clue.sentence &&
                s.stringrepr == e.clue.stringrepr
      }))
        c += 1
      else
        n += 1
    }
    System.err.println(c+"/"+n)


    System.err.print("Clue/Arg-Pair in different sentences: ")
    c = 0
    n = 0
    for(e <- allEvents) {
      if (allEvents.exists(o => {
        e.span.sentence.doc == o.span.sentence.doc &&
                o.clue.sentence != e.clue.sentence &&
                e.clue.stringrepr == o.clue.stringrepr &&
                e.eventType.value == o.eventType.value &&
                e.arguments.exists(ea => o.arguments.exists(oa => oa.entity.span.stringrepr == ea.entity.span.stringrepr))
      })) {
        c += 1
      } else
        n += 1
    }
    System.err.println(c+"/"+n)

    // per event type analysis
    for(tn <- Data.possibleEventTypes) {
      System.err.print("Clue in different sentences for "+tn+" ")
      c = 0
      n = 0
      var k = 0
      for(e <- allEvents ; if (e.eventType.value == tn)) {
        if (allEvents.exists(o => {
          e.span.sentence.doc == o.span.sentence.doc &&
                  o.clue.sentence != e.clue.sentence &&
                  e.clue.stringrepr == o.clue.stringrepr &&
                  e.eventType.value == o.eventType.value
        })) {
          c += 1
        } else {
          n += 1
          if (allSpans.exists(s => s.sentence != e.clue.sentence && s.stringrepr == e.clue.stringrepr)) // string not as event clue existing, but just as string
            k += 1
        }

      }
      System.err.println(c+"/"+n+"("+k+")")
    }

    System.err.println("\nIt seems that there are changes of event type on one string to another in a document")
    var clueStringCounts = new HashMap[String,HashMap[String,Int]]
    for(tn <- Data.possibleEventTypes) {
      //System.err.println("Clue in different sentences for "+tn+" ")
      c = 0
      n = 0
      var k = 0
      var transCounts = new HashMap[String,Int]
      for(e <- allEvents ; if (e.eventType.value == tn)) {
        val otherOpt = allEvents.find(o => { // string equal and diff sentence
          e.span.sentence.doc == o.span.sentence.doc &&
                  o.clue.sentence != e.clue.sentence &&
                  e.clue.stringrepr == o.clue.stringrepr
        })
        if (otherOpt.isDefined && otherOpt.get.clue.sentence.start < e.clue.sentence.start) {
          val trans = otherOpt.get.eventType.value+"-->"+e.eventType.value
          //          System.err.println(trans)
          if (!transCounts.contains(trans)) transCounts += trans -> 0
          transCounts += trans -> (transCounts(trans)+1)
          if (!clueStringCounts.contains(trans)) clueStringCounts += trans -> new HashMap[String,Int]
          if (!clueStringCounts(trans).contains(e.clue.stringrepr)) clueStringCounts(trans) += e.clue.stringrepr -> 0
          clueStringCounts(trans) += e.clue.stringrepr -> (clueStringCounts(trans)(e.clue.stringrepr)+1)
        }
      }
      for(trans <- transCounts.keySet)
        System.err.println(trans+": "+transCounts(trans))
      //      System.err.println
    }
    //    for(trans <- clueStringCounts.keySet) {
    //      System.err.println(trans)
    //      for(c <- clueStringCounts(trans).keySet)
    //        System.err.println(c+"\t"+clueStringCounts(trans)(c))
    //      System.err.println
    //    }

    System.err.println("\nHow does the use of an argument change?")
    var allGeneArgs = for(e <- allEvents ; a <- e.arguments ; if (a.entity.isInstanceOf[Gene])) yield a
    var transCounts = new HashMap[String,Int]
    for(geneArg:Argument <- allGeneArgs) {
      // find all genes which are used as arg more than ones and print the event structure it is used in
      val otherGeneArgOpt = allGeneArgs.find((o:Argument) => {
        geneArg.entity.span.sentence.doc == geneArg.entity.span.sentence.doc &&
                geneArg.entity.span.sentence.start < o.entity.span.sentence.start &&
                geneArg.entity.span.stringrepr == o.entity.span.stringrepr
      })
      if (otherGeneArgOpt.isDefined) {
        //System.err.println(geneArg.entity.span.stringrepr+": "+geneArg.eventWithThisArgument.eventType.value+"-->"+otherGeneArgOpt.get.eventWithThisArgument.eventType.value)
        val trans = geneArg.eventWithThisArgument.eventType.value+"-->"+otherGeneArgOpt.get.eventWithThisArgument.eventType.value
        if (!transCounts.contains(trans)) transCounts += trans -> 0
        transCounts += trans -> (transCounts(trans)+1)
      }
    }
    for(trans <- transCounts.keySet)
      System.err.println(trans+": "+transCounts(trans))

    System.err.println("\nDoes a gene argument get deeper into event structure (only level 2 analyzed)? Print all where that happens.")
    allGeneArgs = for(e <- allEvents ; a <- e.arguments ; if (a.entity.isInstanceOf[Gene])) yield a
    transCounts = new HashMap[String,Int]
    for(geneArg:Argument <- allGeneArgs) {
      // find all genes which are used as arg more than ones and print the event structure it is used in
      val otherGeneArgOpt = allGeneArgs.find((o:Argument) => {
        geneArg.entity.span.sentence.doc == geneArg.entity.span.sentence.doc &&
                geneArg.entity.span.sentence.start < o.entity.span.sentence.start &&
                geneArg.entity.span.stringrepr == o.entity.span.stringrepr &&
                allEvents.exists(e => {
                  e.clue.sentence == o.entity.span.sentence &&
                          e.eventType.value==geneArg.eventWithThisArgument.eventType.value &&
                          e.arguments.exists(a => {a.entity.isInstanceOf[Event] && a.entity.asInstanceOf[Event].arguments.exists(a2 => a2.entity == o.entity)})
                })
      })
      if (otherGeneArgOpt.isDefined) {
        //System.err.println(geneArg.entity.span.stringrepr+": "+geneArg.eventWithThisArgument.eventType.value+"-->"+otherGeneArgOpt.get.eventWithThisArgument.eventType.value)
        val trans = geneArg.eventWithThisArgument.eventType.value+"-->"+otherGeneArgOpt.get.eventWithThisArgument.eventType.value
        if (!transCounts.contains(trans)) transCounts += trans -> 0
        transCounts += trans -> (transCounts(trans)+1)
      }
    }
    for(trans <- transCounts.keySet)
      System.err.println(trans+": "+transCounts(trans))


    System.err.println("\nSame as before, with checking for gene in first sentence.")
    allGeneArgs = for(e <- allEvents ; a <- e.arguments ; if (a.entity.isInstanceOf[Gene])) yield a
    transCounts = new HashMap[String,Int]
    for(geneArg:Argument <- allGeneArgs) {
      // find all genes which are used as arg more than ones and print the event structure it is used in
      val otherGeneArgOpt = allGeneArgs.find((o:Argument) => {
        geneArg.entity.span.sentence.doc == geneArg.entity.span.sentence.doc &&
                geneArg.entity.span.sentence.start < o.entity.span.sentence.start &&
                geneArg.entity.span.stringrepr == o.entity.span.stringrepr &&
                allEvents.exists(e => {
                  e.clue.sentence == o.entity.span.sentence &&
                          e.eventType.value==geneArg.eventWithThisArgument.eventType.value &&
                          e.arguments.exists(a => {a.entity.isInstanceOf[Event] && a.entity.asInstanceOf[Event].arguments.exists(a2 => a2.entity == o.entity)})
                })
      })
      if (otherGeneArgOpt.isDefined) {
        //System.err.println(geneArg.entity.span.stringrepr+": "+geneArg.eventWithThisArgument.eventType.value+"-->"+otherGeneArgOpt.get.eventWithThisArgument.eventType.value)
        val trans = geneArg.eventWithThisArgument.eventType.value+"-->"+otherGeneArgOpt.get.eventWithThisArgument.eventType.value+" (GeneFirstSentence:"+(geneArg.entity.span.sentence.doc.first.genes.exists(g=>g.span.stringrepr==geneArg.entity.span.stringrepr))+")"
        if (!transCounts.contains(trans)) transCounts += trans -> 0
        transCounts += trans -> (transCounts(trans)+1)
      }
    }
    for(trans <- transCounts.keySet)
      System.err.println(trans+": "+transCounts(trans))


    System.err.println("\nSame as before, with checking for gene in first sentence and all regulations together")
    allGeneArgs = for(e <- allEvents ; a <- e.arguments ; if (a.entity.isInstanceOf[Gene])) yield a
    transCounts = new HashMap[String,Int]
    for(geneArg:Argument <- allGeneArgs) {
      // find all genes which are used as arg more than ones and print the event structure it is used in
      val otherGeneArgOpt = allGeneArgs.find((o:Argument) => {
        geneArg.entity.span.sentence.doc == geneArg.entity.span.sentence.doc &&
                geneArg.entity.span.sentence.start < o.entity.span.sentence.start &&
                geneArg.entity.span.stringrepr == o.entity.span.stringrepr &&
                allEvents.exists(e => {
                  e.clue.sentence == o.entity.span.sentence &&
                          e.eventType.value==geneArg.eventWithThisArgument.eventType.value &&
                          e.arguments.exists(a => {a.entity.isInstanceOf[Event] && a.entity.asInstanceOf[Event].arguments.exists(a2 => a2.entity == o.entity)})
                })
      })
      if (otherGeneArgOpt.isDefined) {
        //System.err.println(geneArg.entity.span.stringrepr+": "+geneArg.eventWithThisArgument.eventType.value+"-->"+otherGeneArgOpt.get.eventWithThisArgument.eventType.value)
        val trans =
        (if (geneArg.eventWithThisArgument.eventType.value.contains("egulation")) "Regulation" else geneArg.eventWithThisArgument.eventType.value)+
                "-->"+
                (if (otherGeneArgOpt.get.eventWithThisArgument.eventType.value.contains("egulation")) "Regulation" else otherGeneArgOpt.get.eventWithThisArgument.eventType.value)+
                " (GeneFirstSentence:"+(geneArg.entity.span.sentence.doc.first.genes.exists(g=>g.span.stringrepr==geneArg.entity.span.stringrepr))+")"
        if (!transCounts.contains(trans)) transCounts += trans -> 0
        transCounts += trans -> (transCounts(trans)+1)
      }
    }
    for(trans <- transCounts.keySet)
      System.err.println(trans+": "+transCounts(trans))


  }

  
  def firstSentenceHasEvent(d:Document, withClue:String) : Boolean = {
    if (withClue == null || withClue=="")
      d.first.trueSentence.events.size > 0
    else
      d.first.trueSentence.events.filter(x => x.clue.stringrepr == withClue).size > 0
  }
}
