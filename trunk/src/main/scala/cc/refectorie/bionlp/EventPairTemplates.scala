package cc.refectorie.bionlp

import cc.factorie._
import collection.mutable.{ArrayBuffer, HashSet}

/**
 * This file is part of the BioNLP-ST 2009 system FactEE
 * http://code.google.com/p/factee/
 * Authors: Sebastian Riedel, Roman Klinger
 * License: GPL2
 */


object EventPairTemplates {
  var useGenericFeatures = false
  val conjuncts = Set("and", "or", ",")

  def hasConjunctBetween(span1: Span, span2: Span): Boolean = {
    val (l, r) = if (span1.first.position < span2.first.position) (span1, span2) else (span2, span1)
    var i = l.first.position
    val end = r.first.position
    val sentence = span1.sentence
    while (i < end) {
      val token = sentence(i)
      if (conjuncts(token.word)) return true
      i += 1
    }
    false
  }


  def relativePosition(entity1: Entity, entity2: Entity) = {
    entity1.span.first.position - entity2.span.first.position match {
      case x if (x < 0) => "Left"
      case x if (x > 0) => "Right"
      case _ => "Same"
    }
  }

  def relativeDistance(entity1: Entity, entity2: Entity) = {
    Math.abs(entity1.span.first.position - entity2.span.first.position) match {
      case x if (x < 2) => "Very Close"
      case x if (x < 5) => "Close"
      case x if (x < 10) => "MidRange"
      case _ => "Far"
    }
  }

  class SameClueFeatures(event1: Event, event2: Event) extends BinaryVectorVariable[String](Nil) {
    if (event1 != null && event2 != null) {
      if (useGenericFeatures) {
        this += "Type1:%s Type2:%s".format(event1.eventType.value, event2.eventType.value)
      }
      this += "Bias"
      for (arg1: Argument <- event1.arguments; arg2: Argument <- event2.arguments) {
        val (left, right, leftArg, rightArg) = if (arg1.entity.span.first.position < arg2.entity.span.first.position)
          (event1, event2, arg1, arg2) else (event2, event1, arg2, arg1)
        //we only care about the relative position of each arg to the clue, their role, and the event types
        this += "Type1:%s Role1:%s Position1:%s Type2:%s Role2:%s Position2:%s".format(
          left.eventType.value, leftArg.roleType.value, relativePosition(leftArg.entity, event1),
          right.eventType.value, rightArg.roleType.value, relativePosition(rightArg.entity, event1))
        //        for (feature <- StanfordDependencies.featureFromPath(leftArg.entity.span.first, rightArg.entity.span.first,
        //          Array(1), Array(), false, "")) {
        //          this += "Type1:%s Role1:%s Position1:%s Type2:%s Role2:%s Position2:%s Path:%s".format(
        //            left.eventType.value, leftArg.roleType.value, relativePosition(leftArg.entity, event1),
        //            right.eventType.value, rightArg.roleType.value, relativePosition(rightArg.entity, event1),
        //            feature)
        //        }
        //        this += "Type1:%s Role1:%s Position1:%s Type2:%s Role2:%s Position2:%s Dist:%s".format(
        //          left.eventType.value, leftArg.roleType.value, relativePosition(leftArg.entity, event1),
        //          right.eventType.value, rightArg.roleType.value, relativePosition(rightArg.entity, event1),
        //          relativeDistance(leftArg.entity, rightArg.entity))
        //        this += "Type1:%s Role1:%s Position1:%s Type2:%s Role2:%s Position2:%s Dist:%s Conj:%s".format(
        //          left.eventType.value, leftArg.roleType.value, relativePosition(leftArg.entity, event1),
        //          right.eventType.value, rightArg.roleType.value, relativePosition(rightArg.entity, event1),
        //          relativeDistance(leftArg.entity, rightArg.entity),
        //          hasConjunctBetween(leftArg.entity.span, rightArg.entity.span))
      }
    }

    override def skipNonCategories: Boolean = true
  }

  private def id(event: Event) = {
    event.uniqueRepresentation
    //System.identityHashCode(event)
  }

  object SameClueEventsTemplate extends EventPairTemplate[SameClueFeatures] {
    val allFeatures = new HashSet[String]

    def unrollEvent1(e1: Event): Iterable[Factor] = {
      for (e2 <- e1.clue.sentence.events.filter(e => e.clue == e1.clue && id(e) < id(e1))) yield
        Factor(e1, e2)
    }

    def unrollEvent2(e2: Event): Iterable[Factor] = {
      for (e1 <- e2.clue.sentence.events.filter(e => e.clue == e2.clue && id(e) > id(e2))) yield
        Factor(e1, e2)

    }

    def statistics(e1: Event, e2: Event): Iterable[Stat] = {
      val features = new SameClueFeatures(e1, e2)
      allFeatures ++= features.values
      Stat(features)
    }


    def weightsToString: Iterable[String] = {
      (for (feature <- allFeatures) yield {
        val vector = new SameClueFeatures(null, null)
        vector += feature
        "%-35s %6f".format(feature, weights dot vector.vector)
      }).toList.sort((x, y) => x < y)
    }

    Domain[SameClueFeatures].maxSize = 20000
    init
  }

  class SameArgFeatures(factor: SameArgEventsTemplate.ArgPairFactor) extends BinaryVectorVariable[String](Nil) {
    if (factor != null) {
      //this += "bias"
      val (l, lArg, r, rArg) = if (factor.event1.span.first.position < factor.event2.span.first.position)
        (factor.event1, factor.arg1, factor.event2, factor.arg2) else
        (factor.event2, factor.arg2, factor.event1, factor.arg1)
      this += "Type1:%s Role1:%s Position1:%s Type2:%s Role2:%s Position2:%s".format(
        l.eventType.value, lArg.roleType.value, relativePosition(l, lArg.entity),
        r.eventType.value, rArg.roleType.value, relativePosition(r, lArg.entity))
      this += "Role1:%s Role2:%s".format(lArg.roleType.value, rArg.roleType.value)
      this += "Role1:%s Position1:%s Role2:%s Position2:%s".format(
        lArg.roleType.value, relativePosition(l, lArg.entity),
        rArg.roleType.value, relativePosition(r, lArg.entity))
      this += "Role1:%s Role2:%s ArgIsEvent:%s".format(lArg.roleType.value, rArg.roleType.value,
        lArg.entity.isInstanceOf[Event])
      this += "Role1:%s Position1:%s Role2:%s Position2:%s ArgIsEvent:%s".format(
        lArg.roleType.value, relativePosition(l, lArg.entity),
        rArg.roleType.value, relativePosition(r, lArg.entity),
        lArg.entity.isInstanceOf[Event])

      //      if (!hasConjunctBetween(l.span, r.span))
      //        this += "Conjunct: %6s %6s %6s %6s %6s".format(
      //          lArg.roleType.value, relativePosition(l, lArg.entity),
      //          rArg.roleType.value, relativePosition(r, lArg.entity),
      //          hasConjunctBetween(l.span, r.span))

    }
    override def skipNonCategories: Boolean = true

  }


  object SameArgEventsTemplate extends EventPairTemplate[SameArgFeatures] {
    val allFeatures = new HashSet[String]

    case class ArgPairFactor(event1: Event, arg1: Argument, event2: Event, arg2: Argument)
            extends Factor(event1, event2) {
      override def hashCode: Int = {
        var result = event1.hashCode
        result = result * 31 + event2.hashCode
        result = result * 31 + arg1.hashCode
        result = result * 31 + arg2.hashCode
        result
      }


      override def equals(that: Any): Boolean = {
        that match {
          case f: ArgPairFactor => {
            event1 == f.event1 && event2 == f.event2 && arg1 == f.arg1 && arg2 == f.arg2
          }
          case _ => false
        }
      }
    }


    def unrollEvent1(e1: Event): Iterable[Factor] = {
      for (e2 <- e1.clue.sentence.events; if (e1.span.first.position < e2.span.first.position);
           a1 <- e1.arguments; a2 <- e2.arguments; if (a1.entity == a2.entity)) yield
        ArgPairFactor(e1, a1, e2, a2)

    }

    def unrollEvent2(e2: Event): Iterable[Factor] = {
      for (e1 <- e2.clue.sentence.events; if (e2.span.first.position > e1.span.first.position);
           a1 <- e1.arguments; a2 <- e2.arguments; if (a1.entity == a2.entity)) yield
        ArgPairFactor(e1, a1, e2, a2)

    }

    def statistics(e1: Event, e2: Event): Iterable[Stat] = {
      error("Statistics should never be created with the 2 arg method")
    }


    override def _statistics(factor: Factor): Iterable[Stat] = {
      val features = new SameArgFeatures(factor.asInstanceOf[ArgPairFactor])
      allFeatures ++= features.values
      Stat(features)
    }

    def weightsToString: Iterable[String] = {
      (for (feature <- allFeatures) yield {
        val vector = new SameArgFeatures(null)
        vector += feature
        "%-35s %6f".format(feature, weights dot vector.vector)
      }).toList.sort((x, y) => x < y)
    }
    Domain[SameArgFeatures].maxSize = 20000
    init
  }

  class ParentChildFeatures(parent: Event, child: Event) extends BinaryVectorVariable[String](Nil) {
    if (parent != null && child != null) {
      if (useGenericFeatures) {
        this += "Type1:%s Type2:%s".format(parent.eventType.value, child.eventType.value)
      }
      //this += "bias"
      val connectingArg = parent.arguments.find(_.entity == child).get

      //      this += "%s -> %s -> %s".format(parent.eventType.value,connectingArg.roleType.value, child.eventType.value)
      //      this += "%s -> %s -> %s ()".format(parent.eventType.value,connectingArg.roleType.value,
      //        child.eventType.value,relativePosition(parent,child))

      //      for (childArg: Argument <- child.arguments) {
      //        this += "[%s]RoleChild:%s PositionChild:%s %s".format(
      //            connectingArg.roleType.value,
      //            childArg.roleType.value, relativePosition(childArg.entity, child),
      //            relativePosition(childArg.entity, parent))
      //      }
      //
      //      for (parentArg: Argument <- parent.arguments; childArg: Argument <- child.arguments) {
      //        this += "[%s]Role1:%s Position1:%s Role2:%s Position2:%s %s".format(
      //            connectingArg.roleType.value,
      //            parentArg.roleType.value, relativePosition(parentArg.entity, parent),
      //            childArg.roleType.value, relativePosition(childArg.entity, child),
      //            relativePosition(childArg.entity, parent))
      //      }

      for (parentArg: Argument <- parent.arguments; childArg: Argument <- child.arguments;
           if (parentArg.entity != child && parentArg.entity == childArg.entity)) {
        this += "Share: %s %s %s %s %s %s %s %s".format(
          parent.eventType.value, parentArg.roleType, relativePosition(parent, parentArg.entity),
          child.eventType.value, childArg.roleType, relativePosition(child, childArg.entity),
          connectingArg.roleType.value, childArg.entity.isInstanceOf[Gene])
        this += "Share NoPos: %s %s %s %s %s %s".format(
          parent.eventType.value, parentArg.roleType,
          child.eventType.value, childArg.roleType,
          connectingArg.roleType.value,childArg.entity.isInstanceOf[Gene])
        this += "Share NoPos: %s %s %s %s".format(
          parentArg.roleType,
          childArg.roleType,
          connectingArg.roleType.value,childArg.entity.isInstanceOf[Gene])
//
//        this += "Share: %s %s %s %s %s %s %s".format(
//          parent.eventType.value, parentArg.roleType, relativePosition(parent, parentArg.entity),
//          child.eventType.value, childArg.roleType, relativePosition(child, childArg.entity),
//          connectingArg.roleType.value)
//        this += "Share NoPos: %s %s %s %s %s".format(
//          parent.eventType.value, parentArg.roleType,
//          child.eventType.value, childArg.roleType,
//          connectingArg.roleType.value)
//        this += "Share NoPos: %s %s %s".format(
//          parentArg.roleType,
//          childArg.roleType,
//          connectingArg.roleType.value)

        //        this += "Share"
      }

      for (parentArg: Argument <- parent.arguments; childArg: Argument <- child.arguments;
           if (parentArg.entity == child)) {
        //        we only care about the relative position of each arg to the clue, their role, and the event types
        this += "Type1:%s Role1:%s Position1:%s Type2:%s Role2:%s Position2:%s %s".format(
          parent.eventType.value, parentArg.roleType.value, relativePosition(parentArg.entity, parent),
          child.eventType.value, childArg.roleType.value, relativePosition(childArg.entity, child),
          relativePosition(childArg.entity, parent))
        this += "Role1:%s Position1:%s Role2:%s Position2:%s %s".format(
          parentArg.roleType.value, relativePosition(parentArg.entity, parent),
          childArg.roleType.value, relativePosition(childArg.entity, child),
          relativePosition(childArg.entity, parent))
      }
    }


    override def skipNonCategories: Boolean = true
  }


  object ParentChildEventsTemplate extends EventPairTemplate[ParentChildFeatures] {
    val allFeatures = new HashSet[String]

    def unrollEvent1(e1: Event): Iterable[Factor] = {
      for (e2 <- e1.arguments.filter(_.entity.isInstanceOf[Event]).map(_.entity.asInstanceOf[Event])) yield
        Factor(e1, e2)
    }

    def unrollEvent2(e2: Event): Iterable[Factor] = {
      for (e1 <- e2.clue.sentence.events.filter(e => e.arguments.exists(a => a.entity == e2))) yield
        Factor(e1, e2)

    }

    def statistics(e1: Event, e2: Event): Iterable[Stat] = {
      val features = new ParentChildFeatures(e1, e2)
      allFeatures ++= features.values
      Stat(features)
    }


    def weightsToString: Iterable[String] = {
      (for (feature <- allFeatures) yield {
        val vector = new ParentChildFeatures(null, null)
        vector += feature
        "%-35s %6f".format(feature, weights dot vector.vector)
      }).toList.sort((x, y) => x < y)
    }

    Domain[ParentChildFeatures].maxSize = 20000
    init


  }

  trait EventPairTemplate[T <: DiscreteValues] extends Template2[Event, Event] with DotStatistics1[T] {
    def collectGoldFeatures(docs: Seq[Document]) {
      for (doc <- docs; sentence <- doc; event: Event <- sentence.trueSentence.events) {
        for (factor <- unroll1(event)) {
          _statistics(factor)
        }
      }
      super.freezeDomains
    }

    override def freezeDomains: Unit = {}

    def unrollEvent1(e1: Event): Iterable[Factor]

    def unrollEvent2(e2: Event): Iterable[Factor]

    def unroll1(e1: Event): Iterable[Factor] = {
      if (e1.present) unrollEvent1(e1) else Nil
    }

    def unroll2(e2: Event): Iterable[Factor] = {
      if (e2.present) unrollEvent1(e2) else Nil
    }


  }

  class NaiveDocumentWideVector(val features: Iterable[String]) extends BinaryVectorVariable(features) {
    override def skipNonCategories: Boolean = true
  }

  object NaiveDocumentWideTemplate extends EventPairTemplate[NaiveDocumentWideVector] {
    def firstSentenceWithEvent(sentence: Sentence): Option[Sentence] = {
      for (s <- sentence.doc)
        if (!s.events.isEmpty) return Some(s)
      None

    }

    def unrollEvent1(e1: Event): Iterable[Factor] = {
      val first = firstSentenceWithEvent(e1.span.sentence)
      if (first.isDefined && e1.span.sentence == first.get) return Nil
      for (first <- firstSentenceWithEvent(e1.span.sentence).toList;
           e2 <- first.events) yield Factor(e1, e2)

      //      }
      //      for (sentence <- e1.span.sentence.doc.take(e1.span.sentence.position-1);
      //           e2 <- sentence.events)
      //      yield Factor(e1, e2)

    }

    def unrollEvent2(e2: Event): Iterable[Factor] = {
      val first = firstSentenceWithEvent(e2.span.sentence)
      if (first.isDefined && e2.span.sentence != first.get) return Nil
      if (first.isDefined) for (sentence <- e2.span.sentence.doc.drop(1);
                                e1 <- sentence.events) yield Factor(e1, e2)
      else Nil
      //      val first = firstSentenceWithEvent(e1.event.span.sentence)
      //      for (sentence <- e2.span.sentence.doc.drop(e2.span.sentence.position);
      //           e1 <- sentence.events)
      //      yield Factor(e1, e2)

    }

    def statistics(e1: Event, e2: Event): Iterable[Stat] = {
      val features = new ArrayBuffer[String]
      features += "Types: %s %s".format(e1.eventType.value, e2.eventType.value)
      for (arg1: Argument <- e1.arguments; arg2: Argument <- e2.arguments) {
        features += "Types/Roles: %s/%s %s/%s".format(
          e1.eventType.value, arg1.roleType.value, e2.eventType.value, arg2.roleType)
        features += "Types/Roles/Contained: %s/%s/%s %s/%s/%s".format(
          e1.eventType.value, arg1.roleType.value, arg1.entity.span.stringrepr.contains(arg2.entity.span.stringrepr),
          e2.eventType.value, arg2.roleType.value, arg2.entity.span.stringrepr.contains(arg1.entity.span.stringrepr))
      }
      //      features += "Types: %s %s".format(e1.eventType.value, e2.eventType.value)
//      features += "Types SameStem: %s %s %s".format(e1.eventType.value, e2.eventType.value,
//        e1.clue.mainClueToken.stem == e2.clue.mainClueToken.stem)
//      for (arg1: Argument <- e1.arguments; arg2: Argument <- e2.arguments) {
//        features += "Types/Roles/Contained: %s/%s/%s %s/%s/%s".format(
//          e1.eventType.value, arg1.roleType.value, arg1.entity.span.stringrepr.contains(arg2.entity.span.stringrepr),
//          e2.eventType.value, arg2.roleType.value, arg2.entity.span.stringrepr.contains(arg1.entity.span.stringrepr))
//        features += "Reg Types/Roles/Contained: %s/%s/%s %s/%s/%s".format(
//          e1.eventType.value.endsWith("egulation"), arg1.roleType.value, arg1.entity.span.stringrepr.contains(arg2.entity.span.stringrepr),
//          e2.eventType.value.endsWith("egulation"), arg2.roleType.value, arg2.entity.span.stringrepr.contains(arg1.entity.span.stringrepr))
//        features += "Types/Roles/Contained SameStem: %s/%s/%s %s/%s/%s %s".format(
//          e1.eventType.value, arg1.roleType.value, arg1.entity.span.stringrepr.contains(arg2.entity.span.stringrepr),
//          e2.eventType.value, arg2.roleType.value, arg2.entity.span.stringrepr.contains(arg1.entity.span.stringrepr),
//          e1.clue.mainClueToken.stem == e2.clue.mainClueToken.stem)
//        features += "Reg Types/Roles/Contained SameStem: %s/%s/%s %s/%s/%s %s".format(
//          e1.eventType.value.endsWith("egulation"), arg1.roleType.value, arg1.entity.span.stringrepr.contains(arg2.entity.span.stringrepr),
//          e2.eventType.value.endsWith("egulation"), arg2.roleType.value, arg2.entity.span.stringrepr.contains(arg1.entity.span.stringrepr),
//          e1.clue.mainClueToken.stem == e2.clue.mainClueToken.stem)
//      }

      Stat(new NaiveDocumentWideVector(features))
    }

    override def freezeDomains: Unit = {}

    init

    Domain[NaiveDocumentWideVector].maxSize = 10000

  }


}