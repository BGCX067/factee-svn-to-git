package cc.refectorie.bionlp

import java.lang.String
import cc.factorie.{Domain, DotStatistics1, BinaryVectorVariable, Template1}
import collection.mutable.{HashSet, HashMap, ArrayBuffer}
import io.Source

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Author: Sebastian Riedel
 * License: GPL2
 */

object SyntacticPatternTemplateComposed extends Template1[Event] with DotStatistics1[SyntacticPatternFeatures] {
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

  trait ClueArgPatternGenerator extends PatternGenerator {
    def generate(event: Event, arg: Argument): Seq[Pattern]

    def generateFiltered(event: Event, arg: Argument): Seq[Pattern] = {
      for (p: Pattern <- generate(event, arg);
           if (p.features.exists((f: String) => counts.getOrElse(f, 0) > cutoff))) yield p
    }

    def extractPaths(event: Event, arg: Argument): Seq[Seq[(SDependency, Boolean)]] =
      extractPath(event, arg).toList

    def extractPath(event: Event, arg: Argument): Option[Seq[(SDependency, Boolean)]] =
      event.span.sentence.path(event.span.mainClueToken, arg.entity.span.head)

    def generate(event: Event): Seq[Pattern] = event.arguments.flatMap(arg => generate(event, arg))

    def argRep(event: Event, arg: Argument) = {
      if (arg.entity.isInstanceOf[Gene]) "PROT" else arg.entity.span.head.stem
    }
  }

  trait ClueRepr {
    def clueRepr(event: Event): String
  }

  trait ArgRepr {
    def argRepr(event: Event, arg: Argument): String
  }

  trait PathExtractor {
    def paths(event: Event, arg: Argument): Seq[Seq[(SDependency, Boolean)]]
  }

  trait PathNorm {
    def normalize(path: Seq[(SDependency, Boolean)]): Seq[(SDependency, Boolean)]
  }

  object AnyPath extends PathRepr {
    def pathRepr(path: Seq[(SDependency, Boolean)]): String = "Any"
  }

  object FullPath extends PathExtractor {
    def paths(event: Event, arg: Argument): Seq[Seq[(SDependency, Boolean)]] = {
      event.span.sentence.path(event.span.mainClueToken, arg.entity.span.head).toList
    }
  }

  class NGramPath(n: Int) extends PathExtractor {
    def paths(event: Event, arg: Argument): Seq[Seq[(SDependency, Boolean)]] = {
      for (path: Seq[(SDependency, Boolean)] <- event.span.sentence.path(event.span.mainClueToken, arg.entity.span.head).toList;
           i <- 0 until path.size)
      yield path.slice(i, i + n)
    }
  }

  object PathStart extends PathExtractor {
    def paths(event: Event, arg: Argument): Seq[Seq[(SDependency, Boolean)]] = {
      for (path: Seq[(SDependency, Boolean)] <- event.span.sentence.path(event.span.mainClueToken, arg.entity.span.head).toList;
           i <- 0 until path.size) yield
        path.slice(0, i)
    }
  }
  object PathEnd extends PathExtractor {
    def paths(event: Event, arg: Argument): Seq[Seq[(SDependency, Boolean)]] = {
      for (path: Seq[(SDependency, Boolean)] <- event.span.sentence.path(event.span.mainClueToken, arg.entity.span.head).toList;
           i <- 0 to path.size) yield
        path.slice(i, path.size)
    }
  }

  object PathPrinter extends PathRepr {
    def pathRepr(path: Seq[(SDependency, Boolean)]): String = {
      path.map(edge => if (edge._2) edge._1.dependencyType else edge._1.dependencyType + "-").mkString(" ")
    }
  }

  object StemClueRepr extends ClueRepr {
    def clueRepr(event: Event): String = event.span.mainClueToken.stem
  }

  object StemArgRepr extends ArgRepr {
    def argRepr(event: Event, arg: Argument): String =
      if (arg.entity.isInstanceOf[Gene]) "PROTEIN" else arg.entity.span.head.stem
  }

  object WordArgRepr extends ArgRepr {
    def argRepr(event: Event, arg: Argument): String =
      if (arg.entity.isInstanceOf[Gene]) "PROTEIN" else arg.entity.span.head.word
  }

  object PathLengthRepr extends PathRepr {
    def pathRepr(path: Seq[(SDependency, Boolean)]): String = path.size match {
      case x if (x < 5) => x.toString
      case _ => "LONG"
    }
  }

  object WordClueRepr extends ClueRepr {
    def clueRepr(event: Event): String = event.span.mainClueToken.word
  }

  trait PathRepr {
    def pathRepr(path: Seq[(SDependency, Boolean)]): String
  }

  trait TypeRepr {
    def typeRepr(event: Event): String
  }

  trait RoleRepr {
    def roleRepr(event: Event, arg: Argument): String
  }

  object SimpleRoleRepr extends RoleRepr {
    def roleRepr(event: Event, arg: Argument): String = arg.roleType.value
  }

  object FullTypeRepr extends TypeRepr {
    def typeRepr(event: Event): String = event.eventType.value
  }

  object AnyType extends TypeRepr {
    def typeRepr(event: Event): String = "Any"
  }

  object AnyClue extends ClueRepr {
    def clueRepr(event: Event): String = "Any"
  }

  object AnyArg extends ArgRepr {
    def argRepr(event: Event, arg: Argument): String = "Any"
  }

  object RegTypeRepr extends TypeRepr {
    def typeRepr(event: Event): String = event.eventType.value match {
      case x if (x.endsWith("egulation")) => "Regulation"
      case x => x
    }
  }

  case class ComposedClueArgGenerator(override val name: String,
                                      typeRepr: TypeRepr,
                                      clueRepr: ClueRepr,
                                      pathExtract: PathExtractor,
                                      pathNorm: PathNorm,
                                      pathRepr: PathRepr,
                                      roleRepr: RoleRepr,
                                      argRepr: ArgRepr) extends ClueArgPatternGenerator {
    def generate(event: Event, arg: Argument): Seq[Pattern] = {
      for (path <- pathExtract.paths(event, arg)) yield {
        new Pattern("[%s]%s:%s->%s->%s:%s".format(name,
          clueRepr.clueRepr(event), typeRepr.typeRepr(event),
          pathRepr.pathRepr(pathNorm.normalize(path)), argRepr.argRepr(event, arg), roleRepr.roleRepr(event, arg)))
      }
    }
  }

  case class ComposedClueGenerator(override val name: String,
                                   typeRepr: TypeRepr,
                                   clueRepr: ClueRepr) extends PatternGenerator {
    def generate(event: Event): Seq[Pattern] = {
      Seq(new Pattern("[%s]%s:%s".format(name, clueRepr.clueRepr(event), typeRepr.typeRepr(event))))
    }
  }

  object PathAsIs extends PathNorm {
    def normalize(path: Seq[(SDependency, Boolean)]): Seq[(SDependency, Boolean)] = path
  }

  object PathEquivalenceNormalizer extends PathNorm {
    def normalize(path: Seq[(SDependency, Boolean)]): Seq[(SDependency, Boolean)] = {
      if ((path.last._1.dependencyType == "appos" ||
              path.last._1.dependencyType == "abbrev") && path.last._2)
        path.take(path.size - 1) else
        path
    }
  }

  object PathConjNormalizer extends PathNorm {
    def normalize(path: Seq[(SDependency, Boolean)]): Seq[(SDependency, Boolean)] = {
      path.map(edge => if (edge._1.dependencyType.startsWith("conj"))
        (edge._1.changeType("conj"), edge._2) else edge)
    }
  }


  abstract class PathReprTest(val name: String) extends PathRepr {
    def test(path: Seq[(SDependency, Boolean)]): Boolean


    def pathRepr(path: Seq[(SDependency, Boolean)]): String = {
      if (test(path)) name else "NOT-" + name
    }
  }

  object UpwardDownward extends PathReprTest("UpDown") {
    def test(path: Seq[(SDependency, Boolean)]): Boolean = {
      if (path.size == 0) return false
      if (path(0)._2) return false
      for (edge <- path.drop(0)) {if (edge._2) return true}
      return false
    }
  }

  object ArgParentRepr extends ArgRepr {
    def argRepr(event: Event, arg: Argument) = {
      (for (parent: Token <- arg.entity.span.sentence.heads(arg.entity.span.head);
            if (parent.position != event.clue.mainClueToken.position))
      yield parent.stem).mkString(" ")
    }
  }

  val chunEvents = new Dictionary("chun", "dictionaries/chun-event.txt")
  val chunClueRepr = new ClueDictRepr(chunEvents)
  val chunArgRepr = new ArgDictRepr(chunEvents)

  val clueArgGenerators = setupClueArgGenerators
  val clueGenerators = setupClueGenerators

  val generators = clueArgGenerators ++ clueGenerators


  def setupClueGenerators: Seq[PatternGenerator] = {
    var id = 0
    def newId = {id += 1; id.toString}
    val result = new ArrayBuffer[PatternGenerator]
    for (typeRepr <- Seq(FullTypeRepr, RegTypeRepr);
         clueRepr <- Seq(StemClueRepr, chunClueRepr))
      result += new ComposedClueGenerator(newId, typeRepr, clueRepr)
    result
  }

  def setupClueArgGenerators: Seq[ClueArgPatternGenerator] = {
    var id = 0
    def newId = {id += 1; id.toString}
    val result = new ArrayBuffer[ClueArgPatternGenerator]
    //    for (typeRepr <- Seq(FullTypeRepr,RegTypeRepr, AnyType);
    //         clueRepr <- Seq(StemClueRepr,chunClueRepr, AnyClue);
    //         pathExtr <- Seq(FullPath,new NGramPath(1),new NGramPath(2));
    //         pathNorm <- Seq(PathAsIs);
    //         pathRepr <- Seq(PathPrinter,PathLengthRepr,AnyPath);
    //         argRepr <- Seq(StemArgRepr,chunArgRepr,AnyArg);
    //         roleRepr <- Seq(SimpleRoleRepr)) {
    //      result += new ComposedClueArgGenerator(newId,typeRepr,clueRepr,pathExtr,pathNorm,pathRepr,roleRepr,argRepr)
    //    }
    result += ComposedClueArgGenerator(newId,
      FullTypeRepr, StemClueRepr, FullPath, PathAsIs, PathPrinter, SimpleRoleRepr, StemArgRepr)

    result += ComposedClueArgGenerator(newId,
      FullTypeRepr, StemClueRepr, PathStart, PathAsIs, PathPrinter, SimpleRoleRepr, AnyArg)

    result += ComposedClueArgGenerator(newId,
      FullTypeRepr, AnyClue, PathEnd, PathAsIs, PathPrinter, SimpleRoleRepr, StemArgRepr)

    result += ComposedClueArgGenerator(newId,
      FullTypeRepr, chunClueRepr, PathStart, PathAsIs, PathPrinter, SimpleRoleRepr, AnyArg)

    result += ComposedClueArgGenerator(newId,
      FullTypeRepr, AnyClue, PathEnd, PathAsIs, PathPrinter, SimpleRoleRepr, chunArgRepr)

    result += new ComposedClueArgGenerator(newId,
      FullTypeRepr, StemClueRepr, FullPath, PathAsIs, PathLengthRepr, SimpleRoleRepr, StemArgRepr) {
      override def cutoff = -1
    }
    result += new ComposedClueArgGenerator(newId,
      FullTypeRepr, chunClueRepr, FullPath, PathAsIs, PathLengthRepr, SimpleRoleRepr, chunArgRepr) {
      override def cutoff = -1
    }


    result
  }

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


  trait DictStemRepr extends PatternGenerator {
    def dict: Dictionary

    override def clueRepr(event: Event): String = {
      if (dict.hasStem(event.clue.mainClueToken.stem))
        "<IN %s>".format(dict.name) else "<NOT IN %s>".format(dict.name)
    }
  }

  class ClueDictRepr(dict: Dictionary) extends ClueRepr {
    def clueRepr(event: Event): String = {
      if (dict.hasStem(event.clue.mainClueToken.stem))
        "<%s>".format(dict.name) else "<!%s>".format(dict.name)
    }
  }
  class ArgDictRepr(dict: Dictionary) extends ArgRepr {
    def argRepr(event: Event, arg: Argument): String = {
      if (arg.entity.isInstanceOf[Gene]) "<PROT>" else
      if (dict.hasStem(arg.entity.span.head.stem))
        "<%s>".format(dict.name) else "<!%s>".format(dict.name)
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
      (for (parent: Token <- arg.entity.span.sentence.heads(arg.entity.span.head);
            if (parent.position != event.clue.mainClueToken.position))
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

  override def freezeDomains: Unit = {}

  def extractFeatures(event: Event): Iterable[String] = {
    generators.flatMap(_.generate(event)).flatMap(_.features)
  }

  def extractFilteredFeatures(event: Event): Iterable[String] = {
    generators.flatMap(_.generateFiltered(event)).flatMap(_.features)
  }

  def extractFilteredPatterns(event: Event, arg: Argument): Iterable[Pattern] = {
    clueArgGenerators.flatMap(_.generateFiltered(event, arg))
  }

  def extractFilteredCluePatterns(event: Event): Iterable[Pattern] = {
    clueGenerators.flatMap(_.generateFiltered(event))
  }

  var cutoff = 1

  val counts = new HashMap[String, Int]

  def collectFeatures(docs: Seq[Document]) {
    for (doc <- docs; sentence <- doc; event <- sentence.trueSentence.events) {
      for (feature <- extractFeatures(event)) {
        counts(feature) = counts.getOrElse(feature, 0) + 1
      }
    }
    var overCutoff = 0
    for (entry <- counts) {if (entry._2 > cutoff) overCutoff += 1}
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


}