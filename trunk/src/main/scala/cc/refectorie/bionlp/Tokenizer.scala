package cc.refectorie.bionlp

import collection.mutable.ArrayBuffer
import scala.io.Source
import System.err
import java.io.File

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Roman Klinger
 * License: GPL2
 */

object Tokenizer {

	def tokenizeAndSplitFulltext(docTxt:String,noTokenize:Boolean) : String = {
			val sb = new StringBuilder()
			val doc = sentenceSplit(docTxt)
			for(s <- doc) {
				val tokens = tokenize(s.getText())
        sb ++= "<s> "
        if (noTokenize) {
          sb ++= s.getText()
        } else {
          for(t <- tokens) {
            sb ++= t+" "
          }
        }
				sb ++= " </s>\n"
			}
			sb.toString
	}


	def sentenceSplit(txt:String) : Document = {
			val si = java.text.BreakIterator.getSentenceInstance
			var doc = new Document("NON-ID")
			si.setText(txt)
			var l = 0 ;
			var b = -2 ;
			while(b != -1) {
				b = si.next(1)
				if (b != -1) {
					var sentenceTxt = txt.substring(l, b)
          var sentence = new Sentence(l, b, txt,0,doc)
					doc += sentence
					l = b
				}
			}
			doc
	}

	// 	/* Tokenizes simply by keeping letters together and not using white space. */
	def tokenize(s:String) : Array[String]  = {
			import java.util.Scanner
			val tokens = new ArrayBuffer
			val scanner = new Scanner(s)
			var r = new ArrayBuffer[String];
			while (scanner.hasNext()) {
				scanner.findInLine("[a-zA-Z0-9]+|[^ ]") // or "[a-zA-Z0-9-]+|[^ ]"
				val result = scanner.`match`()
				r += result.group()
			}
			r.toArray
	}

	def main(args: Array[String]) : Unit = {
    if (args.length == 0) {
      System.err.println("Specifiy only file or folder, the last parameter needs to be 'tokenize' or 'untokenized'")
    }
			if (args.length == 2) {
				val docTxt = Source.fromFile(args(0)).collect.mkString.replaceAll("[\n\t\r ]"," ") ;
				val output = tokenizeAndSplitFulltext(docTxt,args(1)=="untokenized")
				println(output)
			}
			else if (args.length == 3) {
				for(f <- new File(args(0)).listFiles ; if (f.getName().endsWith(".txt"))) {
					if (f.isDirectory()) {
						System.err.println("If you specify two parameters, both need to be folders (source and destination)!")
						exit(1)
					}
					val destFileName = args(1)+"/"+f.getName()
					System.err.println("reading from "+f.getAbsoluteFile+"; writing to "+destFileName)
					val docTxt = Source.fromFile(f).collect.mkString.replaceAll("[\n\t\r ]"," ") ;
					val output = tokenizeAndSplitFulltext(docTxt,args(2)=="untokenized")
					val outputstream = new java.io.FileWriter(destFileName)
					outputstream.write(output)
					outputstream.close
				}
			}
			else
				System.err.println("Specify a file, output will be written to stdout\nOR Specify an input folder and an output folder!")
	}
}