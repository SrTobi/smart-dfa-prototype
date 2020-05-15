package de.srtobi.dfaTest
package dfa
package impl

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint
import de.srtobi.dfaTest.dfa.impl.constraints.Constraint._

import scala.annotation.tailrec

trait Facts {
  def truthValueOf(value: DfValue): TruthValue
  def isTrue(value: DfValue): Boolean = truthValueOf(value) == TruthValue.True
  def isFalse(value: DfValue): Boolean = truthValueOf(value) == TruthValue.False

  def withConstraint(constraint: Constraint): Facts
  def claim(value: DfValue): (Option[Facts], Option[Facts])

  def unify(other: Facts): Facts
}


object Facts {
  val empty: Facts = FactsImpl(Map.empty, Set.empty)

  implicit val unifiable: Unifiable[Facts] =
    (entities: IterableOnce[Facts]) => entities.iterator.reduce(_ unify _)
}

case class FactsImpl(claims: Map[DfValue, Boolean], constraints: Set[Constraint]) extends Facts {
  override def truthValueOf(value: DfValue): TruthValue = ???

  override def withConstraint(constraint: Constraint): Facts =
    copy(constraints = constraints + constraint)

  override def claim(value: DfValue): (Option[Facts], Option[Facts]) = {
    val whenTrue = Some(copy(claims = claims + (value -> true)))
      .filterNot(_.isContradiction)
    val whenFalse = Some(copy(claims = claims + (value -> false)))
      .filterNot(_.isContradiction)

    assert(whenTrue.isDefined || whenFalse.isDefined)
    (whenTrue, whenFalse)
  }

  def isContradiction: Boolean = {
    def applyConstraint(constraint: Constraint, targetTruthValue: Boolean, equalities: EqualityMap)(after: EqualityMap => Boolean): Boolean = {
      @tailrec
      def applyDemand(demand: ConstraintDemand, equalities: EqualityMap, targetTruthValue: Boolean)(after: EqualityMap => Boolean): Boolean = {
        demand match {
          case InvertedDemand(inner) => applyDemand(inner, equalities, !targetTruthValue)(after)
          case TruthyDemand(value) => applyValue(value, targetTruthValue, equalities)(after)
          case EqualityDemand(a, b) => equalities.withEquality(a, b, targetTruthValue).fold(false)(after)
        }
      }

      def applyDemands(demands: Seq[ConstraintDemand], equalities: EqualityMap)(after: EqualityMap => Boolean): Boolean = demands match {
        case demand +: rest =>
          applyDemand(demand, equalities, targetTruthValue = true)(
            applyDemands(rest, _)(after)
          )
        case Seq() => after(equalities)
      }

      constraint
        .propagate(targetTruthValue)
        .iterator
        .exists(applyDemands(_, equalities)(after))
    }

    def applyConstraints(constraints: Seq[Constraint], targetTruthValue: Boolean, equalities: EqualityMap)(after: EqualityMap => Boolean): Boolean = constraints match {
      case next +: rest =>
        applyConstraint(next, targetTruthValue, equalities)(
          applyConstraints(rest, targetTruthValue, _)(after)
        )
      case Seq() => after(equalities)
    }

    def applyValue(value: DfValue, targetTruthValue: Boolean, equalities: EqualityMap)(after: EqualityMap => Boolean): Boolean = value match {
      case normal: DfAbstractAny =>
        normal.truthValue.canBe(targetTruthValue) && after(equalities)
      case DfAE(ae) =>
        ae match {
          case pin: PinnedValue =>
            equalities.withTruthValue(pin, targetTruthValue).fold(false)( em =>
              if (em eq equalities) after(em)
              else applyConstraints(constraints.filter(_.result == pin).toSeq, targetTruthValue, em)(after)
            )
          case union: UnionValue =>
            union.values.exists(applyValue(_, targetTruthValue, equalities)(after))
        }
    }

    def applyClaims(claims: Seq[(DfValue, Boolean)], equalities: EqualityMap)(after: EqualityMap => Boolean): Boolean = claims match {
      case (value, targetTruthValue) +: rest =>
        applyValue(value, targetTruthValue, equalities)(
          applyClaims(rest, _)(after)
        )
      case Seq() => after(equalities)
    }

    val foundValidApplication = applyClaims(claims.toSeq, EqualityMap.empty)(_ => true)
    !foundValidApplication
  }

  override def unify(other: Facts): Facts = {
    val o = other.asInstanceOf[FactsImpl]
    val newClaims = claims.combineWith(o.claims) {
      (a, b) => if (a == b) Some(a) else None
    }
    FactsImpl(newClaims, constraints | o.constraints)
  }
}

import de.srtobi.dfaTest.dfa.impl.EqualityMap.Proxy

case class EqualityMap private(var parents: Map[PinnedValue, Proxy],
                               inequalities: Map[PinnedValue, Set[Proxy]],
                               truthyKnowledge: Map[PinnedValue, Boolean]) {

  def withTruthValue(pin: PinnedValue, truthValue: Boolean): Option[EqualityMap] = {
    findProxy(pin) match {
      case Left(concrete) =>
        if (concrete.truthValue.canBe(truthValue)) Some(this)
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
      case _ => Some(this)
    }
  }

  private def withInequality(a: Proxy, b: Proxy): Option[EqualityMap] = {
    if (isEqual(a, b)) None
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
    if (isUnequal(pin, b))
      return None

    Some(withMergedProxies(other, pin))
  }

  private def isEqual(a: Proxy, b: Proxy): Boolean = a == b

  private def isUnequal(pin: PinnedValue, other: Proxy): Boolean = {
    assert(isProxy(pin))

    inequalities.get(pin).exists(_.contains(other))
  }

  private def withMergedProxies(newProxy: Proxy, other: PinnedValue): EqualityMap = {
    assert(isProxy(newProxy))
    assert(isProxy(other))
    assert(truthValueOf(newProxy) overlaps truthValueOf(Right(other)))

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
      truthyKnowledge =
        newProxy.toOption
          .zip(truthyKnowledge.get(other))
          .fold(truthyKnowledge)(
            truthyKnowledge + _
          )
    )
  }

  private def truthValueOf(proxy: PinnedValue): TruthValue = {
    isProxy(proxy)
    TruthValue(truthyKnowledge.get(proxy))
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
  val empty = new EqualityMap(Map.empty, Map.empty, Map.empty)
}