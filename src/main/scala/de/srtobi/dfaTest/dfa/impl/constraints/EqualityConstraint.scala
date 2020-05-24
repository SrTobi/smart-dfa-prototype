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
    } else NoProgress
  }

  override def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Seq[(EqualityMap, Option[Constraint])] =
    (
      for {
        a <- lhs.dfPinnedValues
        b <- rhs.dfPinnedValues
        result <- equalityMap.withEquality(a, b, targetTruthValue)
        if result ne equalityMap
      } yield result -> None
    ).toSeq
}