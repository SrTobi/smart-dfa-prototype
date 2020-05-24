package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint._

case class AndConstraint(conjunctions: Set[Constraint]) extends Constraint {
  assert(conjunctions.size > 1)

  override def applyConstraint(targetTruthValue: Boolean, equalityMap: EqualityMap): Constraint.ApplicationResult = {
    if (targetTruthValue) {
      var progress = false
      val (resultEqualityMap, transformed) =
        conjunctions.foldRight(equalityMap -> Set.empty[Constraint]) {
          case (conjunction, (equalityMap, rest)) =>
            conjunction.applyConstraint(targetTruthValue, equalityMap) match {
              case Tautology =>
                progress = true
                (equalityMap, rest)
              case Applied(equalityMap) =>
                progress = true
                (equalityMap, rest)
              case TransformProgress(equalityMap, newConstraint) =>
                progress = true
                (equalityMap, rest + newConstraint)
              case NoProgress => (equalityMap, rest + conjunction)
              case Contradiction => return Contradiction
            }
        }

      if (!progress) NoProgress
      else {
        def withOutNewConstraints =
          if (resultEqualityMap eq equalityMap) Tautology
          else Applied(resultEqualityMap)
        AndConstraint.tryFrom(transformed)
          .fold(withOutNewConstraints)(TransformProgress(equalityMap, _))
      }
    } else {
      var hadOnlyTautology = true
      for (conjunction <- conjunctions) {
        conjunction.applyConstraint(targetTruthValue, equalityMap) match {
          case Contradiction => return Tautology
          case Tautology =>
          case _ => hadOnlyTautology = false
        }
      }

      if (hadOnlyTautology) Contradiction
      else NoProgress
    }
  }

  override def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Seq[(EqualityMap, Option[Constraint])] = {
    if (targetTruthValue) {
      val result = Seq.newBuilder[(EqualityMap, Option[Constraint])]

      def inner(rest: List[Constraint], equalityMap: EqualityMap, newConstraints: Set[Constraint]): Unit = rest match {
        case next :: rest =>
          for ((resultEqualityMap, newConstraint) <- next.possibleGuesses(targetTruthValue, equalityMap))
            inner(rest, resultEqualityMap, newConstraints ++ newConstraint)
        case Nil =>
          result += (equalityMap -> AndConstraint.tryFrom(newConstraints))
      }

      inner(conjunctions.toList, equalityMap, Set.empty)
      result.result()
    } else {
      conjunctions.iterator.flatMap(_.possibleGuesses(targetTruthValue, equalityMap)).toSeq
    }
  }
}

object AndConstraint {
  def apply(first: Constraint, rest: Constraint*): Constraint =
    apply(Set.from(Iterator(first) ++ rest.iterator))
  def apply(conjunctions: Set[Constraint]): Constraint =
    tryFrom(conjunctions).get

  def tryFrom(conjunctions: Set[Constraint]): Option[Constraint] =
    if (conjunctions.isEmpty) None
    else if (conjunctions.size == 1) Some(conjunctions.head)
    else Some(new AndConstraint(conjunctions))
}