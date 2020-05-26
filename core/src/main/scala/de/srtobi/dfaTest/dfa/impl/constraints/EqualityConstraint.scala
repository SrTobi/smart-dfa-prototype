package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint._

case class EqualityConstraint(lhs: DfValue, rhs: DfValue) extends Constraint {
  override def toString: String = {
    s"($lhs == $rhs)"
  }

  override def applyConstraint(targetTruthValue: Boolean, equalityMap: EqualityMap): Constraint.ApplicationResult = {
    if (!targetTruthValue) {
      var currentEqualityMap = equalityMap
      for {
        a <- lhs.dfPinnedValues
        b <- rhs.dfPinnedValues
      } {
        currentEqualityMap.withEquality(a, b, equal = false) match {
          case Some(newEqualityMap) => currentEqualityMap = newEqualityMap
          case None => return Contradiction
        }
      }
      if (currentEqualityMap eq equalityMap) Tautology
      else Applied(currentEqualityMap)
    } else {
      val result =
        lhs.toDfPinned.zip(rhs.toDfPinned) match {
          case Some((a, b)) => equalityMap.withEquality(a, b)
          case None =>
            val l = equalityMap.normalize(lhs)
            val r = equalityMap.normalize(rhs)
            val intersection = l intersect r
            equalityMap
              .withUpperBound(lhs, intersection)
              .flatMap(_.withUpperBound(rhs, intersection))
        }

        result.fold(Contradiction: ApplicationResult) {
          newEqualityMap =>
            if (newEqualityMap eq equalityMap) NoProgress
            else TransformProgress(newEqualityMap, this)
        }
    }
  }

  override def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Option[Seq[(EqualityMap, Option[Constraint])]] = {
    None
  }
}