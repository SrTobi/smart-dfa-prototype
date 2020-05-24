package de.srtobi.dfaTest
package dfa
package impl

import de.srtobi.dfaTest.dfa.impl.constraints._

import scala.annotation.tailrec
import scala.collection.mutable

trait Facts {
  def computedValues: Map[PinnedValue, DfAbstractAny]

  def withView(pin: PinnedValue, constraint: Constraint): Facts
  def withConditionalPin(pin: PinnedValue, value: DfValue, condition: Constraint): Facts
  def withConstraint(constraint: Constraint): Facts

  def claimConstraint: Option[Constraint]
  def claim(value: DfValue): (Option[Facts], Option[Facts])

  def unify(other: Facts): Facts
}


object Facts {
  val empty: Facts = FactsImpl(Map.empty, Map.empty, Set.empty)

  implicit val unifiable: Unifiable[Facts] =
    (entities: IterableOnce[Facts]) => entities.iterator.reduce(_ unify _)
}

sealed abstract class PinDef {
  def toConstraint(target: PinnedValue): Constraint
}
object PinDef {
  case class View(constraint: Constraint) extends PinDef {
    override def toConstraint(target: PinnedValue): Constraint = {
      ViewConstraint(target, constraint)
    }
  }
  case class Conditional(value: DfValue, condition: Constraint) extends PinDef {
    override def toConstraint(target: PinnedValue): Constraint = {
      ConditionalConstraint(target, value, condition)
    }
  }
}

case class FactsImpl(claims: Map[DfValue, Boolean], pins: Map[PinnedValue, PinDef], constraints: Set[Constraint]) extends Facts {
  override def withConditionalPin(pin: PinnedValue, value: DfValue, condition: Constraint): Facts =
    copy(pins = pins + (pin -> PinDef.Conditional(value, condition)))
  override def withView(pin: PinnedValue, constraint: Constraint): Facts =
    copy(pins = pins + (pin -> PinDef.View(constraint)))
  override def withConstraint(constraint: Constraint): Facts =
    copy(constraints = constraints + constraint)

  override def claimConstraint: Option[Constraint] =
    AndConstraint.tryFrom(claims.iterator.map {
      case (value, true) => TruthyConstraint(value)
      case (value, false) => NotConstraint(TruthyConstraint(value))
    }.toSet)

  override def claim(value: DfValue): (Option[Facts], Option[Facts]) = {
    val whenTrue = Some(copy(claims = claims + (value -> true)))
      .filterNot(_.isContradiction)
    val whenFalse = Some(copy(claims = claims + (value -> false)))
      .filterNot(_.isContradiction)

    assert(whenTrue.isDefined || whenFalse.isDefined)
    if (whenTrue.isDefined && whenFalse.isDefined) (whenTrue, whenFalse)
    else if (whenTrue.isDefined) (Some(this), None)
    else (None, Some(this))
  }

  override def computedValues: Map[PinnedValue, DfAbstractAny] = {
    var result = Map.empty[PinnedValue, DfAbstractAny]
    buildPossibleEqualityMap {
      map =>
        result = result.mergeWith(map.concreteValues)(_ unify _)
        false
    }
    result
  }

  def isContradiction: Boolean = {
    val foundValidApplication = buildPossibleEqualityMap(_ => true)
    !foundValidApplication
  }

  def buildPossibleEqualityMap(after: EqualityMap => Boolean): Boolean = {
    @tailrec
    def applyConstraints(constraints: List[Constraint], initialEqualityMap: EqualityMap): Option[(EqualityMap, List[Constraint])] = {
      var madeProgress = false
      var equalityMap = initialEqualityMap
      val unsatisfiedConstraints = {
        val builder = mutable.Set.empty[Constraint]
        val it = constraints.iterator
        while (it.hasNext) {
          val constraint = it.next()

          constraint.applyConstraint(targetTruthValue = true, equalityMap) match {
            case Constraint.Tautology =>
            case Constraint.Applied(result) =>
              equalityMap = result
              madeProgress = true
            case Constraint.TransformProgress(result, newConstraints) =>
              equalityMap = result
              madeProgress = true
              builder += newConstraints
            case Constraint.NoProgress =>
              builder += constraint
            case Constraint.Contradiction =>
              return None
          }
        }
        builder.toList
      }

      if (!madeProgress) Some(equalityMap -> unsatisfiedConstraints)
      else applyConstraints(unsatisfiedConstraints, equalityMap)
    }

    def inner(constraints: List[Constraint], equalityMap: EqualityMap): Boolean = {
      applyConstraints(constraints, equalityMap) match {
        case None =>
          false
        case Some((equalityMap, Nil)) =>
          after(equalityMap)
        case Some((equalityMap, constraints)) =>
          val constraintsWithGuesses = constraints
            .map(c => c -> c.possibleGuesses(targetTruthValue = true, equalityMap))
            .filter(_._2.nonEmpty)

          val restConstraints = constraintsWithGuesses.map(_._1)

          constraintsWithGuesses.minByOption(_._2.length) match {
            case Some((orginalConstraint,guesses)) =>
              guesses.exists {
                case (guess, maybeConstraint) =>
                  val newConstraints = restConstraints.flatMap {
                      case `orginalConstraint` =>  maybeConstraint
                      case other => Some(other)
                    }
                    .distinct

                  inner(newConstraints, guess)
              }
            case None => after(equalityMap)
          }
      }
    }


    val initialConstraints = (
      constraints.iterator ++
        pins.iterator.map { case (pin, d) => d.toConstraint(pin) } ++
        claims.iterator.map {
          case (v, true) => TruthyConstraint(v)
          case (v, false) => NotConstraint(TruthyConstraint(v))
        }
      ).distinct.toList

    inner(initialConstraints, EqualityMap.empty)
  }

  override def unify(other: Facts): Facts = {
    val o = other.asInstanceOf[FactsImpl]
    val newClaims = claims.combineWith(o.claims) {
      (a, b) => if (a == b) Some(a) else None
    }
    val newPins = pins.mergeWith(o.pins) {
      (a, b) =>
        // todo: this is not correct in loops
        assert(a == b)
        a
    }
    FactsImpl(newClaims, newPins, constraints | o.constraints)
  }
}

import de.srtobi.dfaTest.dfa.impl.EqualityMap.Proxy

case class EqualityMap private(var parents: Map[PinnedValue, Proxy],
                               inequalities: Map[PinnedValue, Set[Proxy]],
                               abstractKnowledge: Map[PinnedValue, DfAbstractAny],
                               truthyKnowledge: Map[PinnedValue, Boolean]) {
  def truthValueOf(value: DfValue): TruthValue = {
    Unifiable.unify(value.dfPinnedValues.map {
      case any: DfAbstractAny => any.truthValue
      case DfPinned(pin) => truthValueOf(pin)
    })
  }

  def normalize(value: DfValue): DfAbstractAny = Unifiable.unify(
    value.dfPinnedValues.map {
      case any: DfAbstractAny => any
      case DfPinned(pin) => findProxy(pin).fold(identity, abstractValuesOf)
    }
  )

  def equalityKnowledge(pin: PinnedValue, other: DfValue): TruthValue = {
    lazy val normalizedOther = normalize(other)
    findProxy(pin) match {
      case Left(concrete) =>
        val intersects = concrete intersects normalizedOther
        if (!intersects) TruthValue.False
        else if (normalizedOther.isConcrete) TruthValue(intersects)
        else TruthValue.Top
      case Right(pin) =>
        other match {
          case DfPinned(otherPin) if pin == otherPin => TruthValue.True
          case _ if (abstractValuesOf(pin) intersects normalizedOther) => TruthValue.Top
          case _ => TruthValue.False
        }
    }
  }

  def concreteValues: Map[PinnedValue, DfAbstractAny] = {
    (parents.keys ++ abstractKnowledge.keySet)
      .map(k => findProxy(k).fold(k -> _, k -> abstractValuesOf(_)))
      .toMap
  }

  def withTruthValue(pin: PinnedValue, truthValue: Boolean): Option[EqualityMap] = {
    findProxy(pin) match {
      case Left(abs) =>
        if (abs.truthValue.canBe(truthValue)) Some(this)
        else None
      case Right(proxy) =>
        truthyKnowledge.get(proxy) match {
          case Some(prior) =>
            if (prior == truthValue) Some(this)
            else None
          case None =>
            Some(copy(truthyKnowledge = truthyKnowledge + (proxy -> truthValue)))
        }
    }
  }

  def withEquality(a: DfPinned, b: DfPinned, equal: Boolean = true): Option[EqualityMap] = {
    (findProxy(a), findProxy(b)) match {
      case (Some(a), Some(b)) =>
        if (equal) withEquality(a, b)
        else withInequality(a, b)
      case (Some(a), None) =>
        if (equal) withEquality(a, b.asInstanceOf[DfAbstractAny])
        else withInequality(a, b.asInstanceOf[DfAbstractAny])
      case (None, Some(b)) =>
        if (equal) withEquality(b, a.asInstanceOf[DfAbstractAny])
        else withInequality(b, a.asInstanceOf[DfAbstractAny])
      case _ =>
        val absA = a.asInstanceOf[DfAbstractAny]
        val absB = b.asInstanceOf[DfAbstractAny]
        if (absA intersects absB) Some(this)
        else None
    }
  }

  private def withInequality(a: Proxy, b: Proxy): Option[EqualityMap] = {
    if (isCertainlyEqual(a, b)) None
    else {
      implicit class InequalitiesOps(val inequalities: Map[PinnedValue, Set[Proxy]]) {
        def addInequality(a: Proxy, b: Proxy): Map[PinnedValue, Set[Proxy]] = a match {
          case Right(pin) => inequalities.updatedWith(pin)(old => Some(old.fold(Set(b))(_ + b)))
          case _ => inequalities
        }
      }
      val newInequalities = inequalities.addInequality(a, b).addInequality(b, a)
      if (newInequalities eq inequalities) Some(this)
      else Some(copy(inequalities = newInequalities))
    }
  }

  private def withInequality(a: Proxy, abstr: DfAbstractAny): Option[EqualityMap] = {
    assert(!abstr.isConcrete)

    // there is nothing much to do, is there?
    // we know that abstr can be one of muiltiple values, but we don't know which exactly
    Some(this)
  }

  private def withEquality(a: Proxy, b: Proxy): Option[EqualityMap] = {
    val (pin, other) = (a, b) match {
      case (Right(pin), b) => (pin, b)
      case (a, Right(pin)) => (pin, a)
      case (Left(a), Left(b)) =>
        return (
          if (a == b) Some(this)
          else None
        )
    }
    if (isCertainlyUnequal(pin, b))
      return None

    Some(withMergedProxies(other, pin))
  }

  private def withEquality(a: Proxy, abstr: DfAbstractAny): Option[EqualityMap] = {
    assert(!abstr.isConcrete)

    a match {
      case Left(concrete) =>
        if (abstr intersects concrete) Some(this)
        else None
      case Right(pin) =>
        abstractKnowledge.get(pin) match {
          case Some(old) =>
            val intersection = old intersect abstr
            if (intersection.isNothing) None
            else Some(copy(
              abstractKnowledge = abstractKnowledge + (pin -> intersection)
            ))
          case None =>
            Some(copy(abstractKnowledge = abstractKnowledge + (pin -> abstr)))
        }


    }
  }

  private def isCertainlyEqual(a: Proxy, b: Proxy): Boolean = a == b

  private def isCertainlyUnequal(pin: PinnedValue, other: Proxy): Boolean = {
    assert(isProxy(pin))

    inequalities.get(pin).exists(_.contains(other))
  }

  private def withMergedProxies(newProxy: Proxy, other: PinnedValue): EqualityMap = {
    assert(isProxy(newProxy))
    assert(isProxy(other))
    assert(truthValueOf(newProxy) <= truthValueOf(Right(other)))
    def intersection = {
      val intersection = abstractValuesOf(newProxy) intersect abstractValuesOf(other)
      assert(!intersection.isNothing)
      intersection
    }

    copy(
      parents = parents + (other -> newProxy),
      inequalities =
        inequalities.get(other)
          .zip(newProxy.toOption)
          .fold(inequalities) {
            case (otherUnequals, newProxy) =>
              inequalities.updatedWith(newProxy) {
                case Some(old) =>
                  Some(old | otherUnequals)
                case None =>
                  Some(otherUnequals)
            }
          },
      abstractKnowledge =
        newProxy.fold(_ => abstractKnowledge, pin =>
          if (intersection == DfAny) abstractKnowledge
          else abstractKnowledge + (pin -> intersection)
        ),
      truthyKnowledge =
        newProxy.toOption
          .zip(truthyKnowledge.get(other))
          .fold(truthyKnowledge)(
            truthyKnowledge + _
          )
    )
  }

  private def abstractValuesOf(pin: PinnedValue): DfAbstractAny =
    abstractKnowledge.getOrElse(pin, DfAny)

  private def abstractValuesOf(proxy: Proxy): DfAbstractAny =
    proxy.fold(identity, abstractValuesOf)

  private def truthValueOf(proxy: PinnedValue): TruthValue = {
    assert(isProxy(proxy))
    val fromAbstractKnowledge = abstractKnowledge.get(proxy).fold(TruthValue.Top: TruthValue)(_.truthValue)
    val fromTruthyKnowledge = TruthValue(truthyKnowledge.get(proxy))
    assert(fromAbstractKnowledge overlaps fromTruthyKnowledge)
    fromAbstractKnowledge unify fromTruthyKnowledge
  }

  private def truthValueOf(proxy: Proxy): TruthValue = proxy match {
    case Left(value) => value.truthValue
    case Right(pin) => truthValueOf(pin)
  }

  private def findProxy(value: DfPinned): Option[Proxy] = value match {
    case DfPinned(pin) => Some(findProxy(pin))
    case concrete: DfConcreteAny => Some(Left(concrete))
    case _ => None
  }

  private def findProxy(pin: PinnedValue): Proxy =
    parents.get(pin) match {
      case Some(parent) =>
        val proxy = findProxy(parent)
        if (proxy != parent) {
          // union find path compression
          parents += pin -> proxy
        }
        proxy
      case None =>
        Right(pin)
    }

  private def findProxy(value: Proxy): Proxy = value match {
    case Right(pin) => findProxy(pin)
    case proxy => proxy
  }

  private def isProxy(proxy: Proxy): Boolean =
    findProxy(proxy) == proxy

  private def isProxy(pin: PinnedValue): Boolean =
    !parents.contains(pin)
}

object EqualityMap {
  private type Proxy = Either[DfConcreteAny, PinnedValue]
  val empty = new EqualityMap(Map.empty, Map.empty, Map.empty, Map.empty)

  case class Info(truthValue: TruthValue = TruthValue.Top,
                  inequalities: Set[PinnedValue] = Set.empty,
                  value: DfAbstractAny = DfAny)
}