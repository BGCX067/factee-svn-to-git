package cc.refectorie.bionlp

import java.lang.String
import collection.mutable.{HashSet, HashMap, ArrayBuffer}
import io.Source
import cc.factorie._

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Sebastian Riedel
 * License: GPL2
 */


class SyntacticPatternFeatures(features: Iterable[String]) extends BinaryVectorVariable[String](features) {
//  System.err.println
//  this.values.foreach(x => System.err.println(x))
//  System.err.println
  override def skipNonCategories: Boolean = true
}


object SyntacticPatternTemplate extends Template1[Event] with DotStatistics1[SyntacticPatternFeatures] {
  class Pattern {
    def this(feature: String*) {
      this ()
      features ++= feature
    }

    def featuresWithStats: Seq[Feature] = {
      features.map(feature => Feature(feature, counts.getOrElse(feature, 0),
        weights dot new SyntacticPatternFeatures(Seq(feature)).vector))
    }

    val features = new ArrayBuffer[String]


    override def toString: String = features.mkString("::")

    def toStringWithStats: String =
      featuresWithStats.map(f => "%s[%d,%4f]".format(f.feature, f.count, f.weight)).mkString("::")
  }

  trait PatternGenerator {
    def name: String = ""

    def generate(event: Event): Seq[Pattern]

    def generateFiltered(event: Event) = {
      for (p: Pattern <- generate(event);
           if (p.features.exists((f: String) => counts.getOrElse(f, 0) > cutoff))) yield p
    }

    def cutoff = SyntacticPatternTemplate.cutoff

    def clueRepr(event: Event): String = event.clue.mainClueToken.word

    def eventTypeRepr(event: Event) = {event.eventType.value}

    def createPattern(features: String*) = {
      val result = new Pattern
      result.features ++= features.map(name + ":" + _)
      result
    }
  }

  trait ClueStemRepr extends PatternGenerator {
    override def clueRepr(event: Event): String = event.clue.mainClueToken.stem.toLowerCase


    override def name: String = super.name + "_CSR"
  }

  trait AnyEventType extends PatternGenerator {
    override def eventTypeRepr(event: Event) = "Any"


    override def name: String = super.name + "_AET"
  }

  trait RegularizedEventType extends PatternGenerator {
    override def eventTypeRepr(event: Event) = event.eventType.value match {
      case t if (t.endsWith("egulation")) => "*Regulation"
      case t => "*" + t
    }

    override def name: String = super.name + "_RET"
  }

  trait ClueArgPatternGenerator extends PatternGenerator {
    def generate(event: Event, arg: Argument): Seq[Pattern]

    def generateFiltered(event: Event, arg: Argument): Seq[Pattern] = {
      for (p: Pattern <- generate(event, arg);
           if (p.features.exists((f: String) => counts.getOrElse(f, 0) > cutoff))) yield p
    }

    def extractPath(event: Event, arg: Argument): Option[Seq[(SDependency, Boolean)]] =
      event.span.sentence.path(event.span.mainClueToken, arg.entity.span.head)

    def generate(event: Event): Seq[Pattern] = event.arguments.flatMap(arg => generate(event, arg))

    def argRep(event: Event, arg: Argument) = {
      if (arg.entity.isInstanceOf[Gene]) "PROT" else arg.entity.span.head.stem
    }

  }

  trait ArgArgPatternGenerator extends PatternGenerator {
    def generate(event: Event, arg1: Argument, arg2: Argument): Seq[Pattern]

    def generateFiltered(event: Event, arg1: Argument, arg2: Argument): Seq[Pattern] = {
      for (p: Pattern <- generate(event, arg1, arg2);
           if (p.features.exists((f: String) => counts.getOrElse(f, 0) > cutoff))) yield p
    }


    def extractPath(arg1: Argument, arg2: Argument): Option[DepPath] =
      arg1.entity.span.sentence.path(arg1.entity.span.head, arg2.entity.span.head).map(new DepPath(_))

    def generate(event: Event): Seq[Pattern] =
      for (left <- event.arguments; right <- event.arguments;
           if (left.entity.span.head.position < right.entity.span.head.position);
           pattern <- generate(event, left, right)) yield pattern

    def argRep(event: Event, arg: Argument) = {
      //if (arg.entity.isInstanceOf[Gene]) "PROT" else arg.entity.span.head.stem
      if (arg.entity.isInstanceOf[Gene]) "PROT" else "NO-PROT"
    }
  }



  class FullPathArgPairGenerator extends ArgArgPatternGenerator {
    def generate(event: Event, arg1: Argument, arg2: Argument): Seq[Pattern] = {
      Seq(createPattern("[%s] %s:%s -> %s -> %s:%s".format(
        event.eventType.value,
        argRep(event, arg1), arg1.roleType.value,
        extractPath(arg1, arg2).map(pathRepr(event, arg1, arg2, _)),
        argRep(event, arg2), arg2.roleType.value)))
    }

    override def name: String = super.name + "_FAP"

    def pathRepr(event: Event, arg1: Argument, arg2: Argument, path: DepPath) = path.toString
  }

  trait AnyArgPairPath extends FullPathArgPairGenerator {
    override def pathRepr(event: Event, arg1: Argument, arg2: Argument, path: DepPath) = "Any"
  }



  trait RelativePositionArgPair extends FullPathArgPairGenerator {
    override def pathRepr(event: Event, arg1: Argument, arg2: Argument, path: DepPath) = {
      "%s/%s".format(
        EventPairTemplates.relativePosition(arg1.entity, event),
        EventPairTemplates.relativePosition(arg2.entity, event))
    }

    override def name: String = super.name + "_RPA"
  }

  class NGramPathArgPairGenerator(n: Int) extends FullPathArgPairGenerator {
    override def generate(event: Event, arg1: Argument, arg2: Argument): Seq[Pattern] = {
      for (path <- extractPath(arg1, arg2).toList; i <- 0 to path.path.size - n) yield {
        createPattern("[%s] %s:%s -> %s -> %s:%s".format(
          event.eventType.value,
          argRep(event, arg1), arg1.roleType.value,
          pathRepr(event, arg1, arg2, new DepPath(path.path.slice(i, i + n))),
          argRep(event, arg2), arg2.roleType.value))
      }
    }

    override def name: String = super.name + "_NAP"

  }

  class PathLength extends ClueArgPatternGenerator {
    def generate(event: Event, arg: Argument): Seq[Pattern] = {
      Seq(createPattern(extractPath(event, arg).map(path => path.size match {
        case x if (x < 5) => x.toString
        case _ => "LONG"
      }).toString))
    }


    override def cutoff = -1
  }

  def binnedSize(size: Int): String = {
    size match {
      case x if (x < 5) => x.toString
      case _ => "LONG"
    }
  }



  trait PathLengthMixIn extends AbstractFullPathPatternGenerator {
    override def pathRepr(path: Seq[(SDependency, Boolean)]): String = {
      path.size match {
        case x if (x < 5) => x.toString
        case _ => "LONG"
      }
    }


    override def cutoff = -1
  }

  trait AnyPath extends AbstractFullPathPatternGenerator {
    override def pathRepr(path: Seq[(SDependency, Boolean)]): String = "Any"

    override def cutoff = -1
  }


  trait PathNormalizer extends ClueArgPatternGenerator {
    abstract override def extractPath(event: Event, arg: Argument): Option[Seq[(SDependency, Boolean)]] = {
      for (path: Seq[(SDependency, Boolean)] <- super.extractPath(event, arg)) yield {
        if (((path.last._1.dependencyType == "appos"
                //                || path.last._1.dependencyType == "abbrev"
                )) && path.last._2)
          path.take(path.size - 1) else
          path
      }
    }


    override def name: String = super.name + "_PN"
  }

  trait ConjunctionNormalizer extends ClueArgPatternGenerator {
    abstract override def extractPath(event: Event, arg: Argument): Option[Seq[(SDependency, Boolean)]] = {
      for (path: Seq[(SDependency, Boolean)] <- super.extractPath(event, arg)) yield
        path.map(edge => if (edge._1.dependencyType.startsWith("conj"))
          (edge._1.changeType("conj"), edge._2) else edge)
    }

    override def name: String = super.name + "_CN"
  }

  trait PrepNormalizer extends ClueArgPatternGenerator {
    abstract override def extractPath(event: Event, arg: Argument): Option[Seq[(SDependency, Boolean)]] = {
      for (path: Seq[(SDependency, Boolean)] <- super.extractPath(event, arg)) yield
        path.map(edge => if (edge._1.dependencyType.startsWith("prep"))
          (edge._1.changeType("prep"), edge._2) else edge)
    }

    override def name: String = super.name + "_PN"
  }



  trait AbstractFullPathPatternGenerator extends ClueArgPatternGenerator {
    def generate(event: Event, arg: Argument): Seq[Pattern] = {
      Seq(createPattern("%s:%s->%s->%s:%s".format(eventTypeRepr(event), clueRepr(event),
        extractPath(event, arg).map(pathRepr(_)).toString,
        arg.roleType.value,
        argRep(event, arg))))
    }

    def pathRepr(path: Seq[(SDependency, Boolean)]): String = fullPath(path)


    override def name: String = super.name + "_FP"
  }


  trait AbstractSplitPathPatternGenerator extends ClueArgPatternGenerator {
    def geneNoGene(arg: Argument) = if (arg.entity.isInstanceOf[Gene]) "PROTEIN" else "NOPROT"

    def generate(event: Event, arg: Argument): Seq[Pattern] = {
      for (path <- extractPath(event, arg).toList;
           i <- 0 to path.size) yield
        createPattern(
          "%s:%s(%s)[%s]->%s".format(eventTypeRepr(event), arg.roleType.value,
            clueRepr(event), fullPath(path.slice(0, i)), geneNoGene(arg)),
          "%s->%s".format(fullPath(path.slice(i, path.size)), argRep(event, arg)))
    }


    override def name: String = super.name + "_IP"
  }

  trait AbstractInnerPathGenerator extends ClueArgPatternGenerator {
    def geneNoGene(arg: Argument) = if (arg.entity.isInstanceOf[Gene]) "PROTEIN" else "NOPROT"

    def generate(event: Event, arg: Argument): Seq[Pattern] = {
      for (path <- extractPath(event, arg).toList;
           i <- 0 until path.size; j <- i + 1 to path.size) yield
        createPattern(
          "%s:%s(%s)[%s]->%s".format(eventTypeRepr(event), arg.roleType.value,
            clueRepr(event), fullPath(path.slice(i, j)), argRep(event, arg)))
    }


    override def name: String = super.name + "_SP"
  }


  trait AnyClueRepr extends PatternGenerator {
    override def clueRepr(event: Event): String = "Any"


    override def name: String = super.name + "_AC"
  }

  abstract class TestPath(accept: String) extends ClueArgPatternGenerator {
    def test(path: Seq[(SDependency, Boolean)]): Boolean

    override def cutoff = -1

    def generate(event: Event, arg: Argument): Seq[Pattern] = {
      (for (path <- event.span.sentence.path(event.span.mainClueToken, arg.entity.span.head)) yield
        createPattern(if (test(path)) accept else "No" + accept)).toList
    }

    override def clueRepr(event: Event): String = ""


    override def name: String = "_PTEST"
  }


  object UpwardDownwardTest extends TestPath("UpDown") {
    def test(path: Seq[(SDependency, Boolean)]): Boolean = {
      if (path.size == 0) return false
      if (path(0)._2) return false
      for (edge <- path.drop(0)) {if (edge._2) return true}
      return false
    }
  }


  object DownwardUpwardTest extends TestPath("DownUp") {
    def test(path: Seq[(SDependency, Boolean)]): Boolean = {
      if (path.size == 0) return false
      if (!path(0)._2) return false
      for (edge <- path.drop(0)) {if (!edge._2) return true}
      return false
    }
  }


  object StartUpwardTest extends TestPath("StartUp") {
    def test(path: Seq[(SDependency, Boolean)]): Boolean = {
      path.size > 0 && !path(0)._2
    }
  }

  object AllUpwardTest extends TestPath("AllUp") {
    def test(path: Seq[(SDependency, Boolean)]): Boolean = {
      path.forall(!_._2)
    }

  }


  object NoGoTest extends ClueArgPatternGenerator with AnyClueRepr {
    def test(path: Seq[(SDependency, Boolean)]): Boolean = {
      path.contains((edge: (SDependency, Boolean)) => edge._1.dependencyType.startsWith("parataxis"))
    }

    override def cutoff = -1

    def generate(event: Event, arg: Argument): Seq[Pattern] = {
      (for (path <- event.span.sentence.path(event.span.mainClueToken, arg.entity.span.head)) yield
        createPattern(if (test(path)) "NoGo" else "Go")).toList
    }
  }


  object FullPathPatternGenerator extends AbstractFullPathPatternGenerator with ClueStemRepr

  object SplitPathPatternGenerator extends AbstractSplitPathPatternGenerator with ClueStemRepr

  object SplitPathDictGenerator extends AbstractSplitPathPatternGenerator with DictStemRepr {
    def dict: Dictionary = chunEvents
  }


  trait AbstractStemTypeGenerator extends PatternGenerator {
    def generate(event: Event): Seq[Pattern] = {
      Seq(createPattern("%s:%s".format(event.eventType.value, clueRepr(event))))
    }


    override def name: String = super.name + "_ST"
  }

  object StemTypeGenerator extends AbstractStemTypeGenerator with ClueStemRepr
  object StemTypeGeneratorDict extends AbstractStemTypeGenerator with DictStemRepr {
    def dict: Dictionary = chunEvents
  }

  object ClueParentGenerator extends AbstractStemTypeGenerator {
    override def clueRepr(event: Event): String = {
      val sentence = event.span.sentence

      //      (for (head:Token <- sentence.head(event.span.mainClueToken);
      //           if (!head.isInGene)) yield {
      //        head.stem
      //      }).mkString(" ")
      (for (headDep: (SDependency, Token) <- sentence.headDeps(event.span.mainClueToken).toList.
              sort((t1, t2) => t1._2.position < t2._2.position)) yield {
        headDep._1.dependencyType
      }).mkString(" ")
    }

    override def name: String = super.name + "_CPG"
  }



  object EventTypeBias extends PatternGenerator {
    def generate(event: Event): Seq[Pattern] = Seq(createPattern("Bias(" + event.eventType.value + ")"))

    override def cutoff = -1
  }

  object ClueArgBias extends ClueArgPatternGenerator {
    def generate(event: Event, arg: Argument): Seq[Pattern] =
      Seq(createPattern("Bias(%s,%s)".format(event.eventType.value, arg.roleType.value)))

    override def cutoff = -1

  }

  trait HyphenRepr extends PatternGenerator {
    override def clueRepr(event: Event): String = {
      val sentence = event.span.sentence
      if (event.span.mainClueToken.position > 1 &&
              event.span.mainClueToken.prev.word == "-")
        event.span.mainClueToken.prev.prev.word
      else
        "NOHYPH"
    }


    override def name: String = super.name + "_HN"
  }


  object HyphenDependentCheck extends ClueArgPatternGenerator {
    val dependent = Set("dependent")

    def generate(event: Event, arg: Argument): Seq[Pattern] = {
      Seq(createPattern("%s:%s->%s->%s:%s".format(eventTypeRepr(event), clueRepr(event),
        extractPath(event, arg).map(pathRepr(event.span.sentence, _)).toString,
        arg.roleType.value,
        argRep(event, arg))))
    }


    def pathRepr(sentence: Sentence, path: Seq[(SDependency, Boolean)]): String = {
      if (path.size == 2 && path(0)._1.dependencyType == "amod" &&
              path(0)._2 && path(1)._1.dependencyType.startsWith("hyphen") && path(1)._2 &&
              dependent(sentence(path(0)._1.tok2).word))
        "HyphenDependent" else "NotHyphenDependent"

    }
  }



  val clueArgGenerators = Seq(
    FullPathPatternGenerator,
    new AbstractFullPathPatternGenerator with AnyEventType with AnyClueRepr,
    new AbstractFullPathPatternGenerator with RegularizedEventType with AnyClueRepr,
    SplitPathPatternGenerator,
    SplitPathDictGenerator,
    new AbstractFullPathPatternGenerator with AnyClueRepr with ArgParentRepr with AnyPath,
    new AbstractSplitPathPatternGenerator with ClueStemRepr with PathNormalizer,
    new AbstractSplitPathPatternGenerator with ClueStemRepr with ConjunctionNormalizer,
    //    new AbstractSplitPathPatternGenerator with ClueStemRepr with PathNormalizer with ConjunctionNormalizer,
    new AbstractSplitPathPatternGenerator with ConjunctionNormalizer with DictArgRepr {
      def dict = chunEvents
    },
    //    NoGoTest,
    new PathLength,
    new AbstractFullPathPatternGenerator with PathLengthMixIn with AnyClueRepr with DictArgRepr {
      def dict = chunEvents
    },
    UpwardDownwardTest,
    //DownwardUpwardTest,
    //    HyphenDependentCheck,
    //        AllUpwardTest,
    //        StartUpwardTest,
    //    new AbstractSplitPathPatternGenerator with DictStemRepr with DictArgRepr {def dict = chunEvents},
    //        new AbstractSplitPathPatternGenerator with AnyEventType with AnyClueRepr,
    //        new AbstractSplitPathPatternGenerator with ClueStemRepr with DictArgRepr {def dict = chunEvents},
    //        new AbstractSplitPathPatternGenerator with RegularizedEventType with DictStemRepr {def dict = chunEvents},
    //    new AbstractInnerPathGenerator with ClueStemRepr with DictArgRepr {def dict = chunEvents},
    //    new AbstractInnerPathGenerator with DictStemRepr with DictArgRepr {def dict = chunEvents},
    ClueArgBias
    )

  val clueGenerators = Seq(
    StemTypeGenerator, StemTypeGeneratorDict,
    new AbstractStemTypeGenerator with HyphenRepr,
    //    ClueParentGenerator,
    EventTypeBias)

  val argpairGenerators = if (InitFile.useArgPairFeatures) Seq[ArgArgPatternGenerator](
    new FullPathArgPairGenerator,
    new NGramPathArgPairGenerator(1),
    new NGramPathArgPairGenerator(2),
    new FullPathArgPairGenerator {
      override def pathRepr(event: Event, arg1: Argument, arg2: Argument, path: DepPath) = binnedSize(path.path.size).toString
    },
    new FullPathArgPairGenerator with RelativePositionArgPair,
    new FullPathArgPairGenerator with AnyArgPairPath
    ) else Nil


  val generators = clueArgGenerators ++ clueGenerators ++ argpairGenerators


  class Dictionary(val name: String) {
    val words = new HashSet[String]
    val stems = new HashSet[String]

    def this(name: String, file: String) {
      this (name)
      load(file)
    }

    def load(file: String) {
      for (line <- Source.fromFile(file).getLines) {
        val split = line.split("\\s+")
        words ++= split.map(_.toLowerCase)
        stems ++= split.map(Data.stem(_).toLowerCase)
      }
    }

    def add(token: Token) {
      words += token.word.toLowerCase
      stems += token.stem.toLowerCase
    }

    def hasWord(word: String) = words(word.toLowerCase)

    def hasStem(stem: String) = stems(stem.toLowerCase)
  }

  object AllPossibleClues extends Dictionary("All") {
    load("dictionaries/chun-event.txt")
    load("dictionaries/chun-manual.txt")
  }

  val chunEvents = new Dictionary("chun", "dictionaries/chun-event.txt")

  trait DictStemRepr extends PatternGenerator {
    def dict: Dictionary

    override def clueRepr(event: Event): String = {
      if (dict.hasStem(event.clue.mainClueToken.stem))
        "<IN %s>".format(dict.name) else "<NOT IN %s>".format(dict.name)
    }
  }

  trait DictArgRepr extends ClueArgPatternGenerator {
    def dict: Dictionary

    override def argRep(event: Event, arg: Argument) = {
      if (arg.entity.isInstanceOf[Gene]) "<PROT>" else
      if (dict.hasStem(arg.entity.span.head.stem))
        "<IN %s>".format(dict.name) else "<NOT IN %s>".format(dict.name)
    }


    override def name: String = super.name + "_DAR"
  }

  trait ArgParentRepr extends ClueArgPatternGenerator {
    override def argRep(event: Event, arg: Argument) = {
      (for (parent: Token <- arg.entity.span.sentence.heads(arg.entity.span.head).toList.
              sort((t1, t2) => t1.position < t2.position);
            if (!parent.isInGene && parent.position != event.clue.mainClueToken.position))
      yield parent.stem).mkString(" ")
    }

    override def name: String = super.name + "_APR"
  }

  trait ProtNoProtArgRepr extends ClueArgPatternGenerator {
    override def argRep(event: Event, arg: Argument) = {
      if (arg.entity.isInstanceOf[Gene]) "PROT" else "NOPROT"
    }

    override def name: String = super.name + "_PNP"
  }

  def fullPath(path: Seq[(SDependency, Boolean)]): String = {
    path.map(edge => edge._1.dependencyType + (if (edge._2) "" else "-")).mkString(" ")
  }


  def statistics(event: Event): Iterable[Stat] = {
    if (event.present)
      Stat(new SyntacticPatternFeatures(extractFilteredFeatures(event)))
    else Nil
  }

  override def freezeDomains: Unit = {
  }

  def extractFeatures(event: Event): Iterable[String] = {
    generators.flatMap(_.generate(event)).flatMap(_.features)
  }

  def extractFilteredFeatures(event: Event): Iterable[String] = {
    generators.flatMap(_.generateFiltered(event)).flatMap(_.features)
  }

  def extractFilteredPatterns(event: Event, arg: Argument): Iterable[Pattern] = {
    clueArgGenerators.flatMap(_.generateFiltered(event, arg))
  }

  def extractFilteredPatterns(event: Event, arg1: Argument, arg2: Argument): Iterable[Pattern] = {
    argpairGenerators.flatMap(_.generateFiltered(event, arg1, arg2))
  }

  def extractFilteredCluePatterns(event: Event): Iterable[Pattern] = {
    clueGenerators.flatMap(_.generateFiltered(event))
  }

  var cutoff = 1

  val counts = new HashMap[String, Int]

  def collectFeatures(docs: Seq[Document]) {
    for (doc <- docs;
         sentence <- doc;
         event <- sentence.trueSentence.events) {
      for (feature <- extractFeatures(event)) {
        counts(feature) = counts.getOrElse(feature, 0) + 1
      }
    }
    var overCutoff = 0
    for (entry <- counts) {
      if (entry._2 > cutoff) overCutoff += 1
    }
    System.err.println("Collected %d syntactic featuresWithStats; %d over cutoff %d".format(counts.size, overCutoff, cutoff))

  }


  Domain[SyntacticPatternFeatures].maxSize = 4000000
  init

  case class Feature(feature: String, count: Int, weight: Double)

  def orderedWeights: Seq[Feature] = {
    (for (feature <- counts) yield {
      Feature(feature._1, feature._2, weights dot new SyntacticPatternFeatures(Seq(feature._1)).vector)
    }).toList.sort((f1, f2) => f1.weight > f2.weight)
  }

  class DepPath(val path: Seq[(SDependency, Boolean)]) {
    override def toString = path.map(
      entry => if (entry._2) entry._1.dependencyType else entry._1.dependencyType + "-").mkString(" ")

    def startEndPaths = {
      (for (i <- 0 to path.size) yield new DepPath(path.slice(0, i))) ++
              (for (i <- 0 to path.size) yield new DepPath(path.slice(i, path.size)))

    }
  }

  class ArgPairFeatures(features: Iterable[String]) extends BinaryVectorVariable(features) {
    override def skipNonCategories: Boolean = true
  }

  object ArgPairTemplate extends Template1[Event] with DotStatistics1[ArgPairFeatures] {
    def statistics(event: Event): Iterable[Stat] = {
      val features = new ArrayBuffer[String]
      features += "DuplicateArg: %s".format(event.arguments.exists(a1 => event.arguments.exists(a2 =>
        a1 != a2 && a1.entity == a2.entity)))
      Stat(new ArgPairFeatures(features))
    }
    init


    override def freezeDomains: Unit = {}
  }

  Domain[ArgPairFeatures].maxSize = 10000


}