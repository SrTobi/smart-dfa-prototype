package de.srtobi.dfaTest
package dfa
package impl

import scala.annotation.tailrec
import scala.collection.mutable

trait Facts {
  def computedValues(necessaryValues: Seq[DfValue]): Map[PinnedValue, DfAbstractAny]

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

  override def computedValues(necessaryValues: Seq[DfValue]): Map[PinnedValue, DfAbstractAny] = {
    var result = Map.empty[PinnedValue, DfAbstractAny]
    buildPossibleEqualityMap {
      map => {
        if (necessaryValues.forall(!map.isNothing(_))) {
          //print("")
          result = result.mergeWithOtherDefaulting(map.concreteValues, DfAny)(_ unify _)
        } else {
          //print("")
        }
        false
      }
    }
    result.toMap
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
            .flatMap(c => c.possibleGuesses(targetTruthValue = true, equalityMap).map(c -> _))
            .filter(_._2.nonEmpty)

          constraintsWithGuesses.minByOption(_._2.length) match {
            case Some((orginalConstraint,guesses)) =>
              guesses.exists {
                case (guess, maybeConstraint) =>
                  val newConstraints = constraints.flatMap {
                      case `orginalConstraint` =>  maybeConstraint
                      case other => Some(other)
                    }
                    .distinct

                  inner(newConstraints, guess)
              }
            case None =>
              after(equalityMap)
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
