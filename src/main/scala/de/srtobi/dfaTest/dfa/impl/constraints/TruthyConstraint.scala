package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint._

case class TruthyConstraint(value: DfValue) extends Constraint {
  override def toString: String = s"truthy[$value]"

  override def applyConstraint(targetTruthValue: Boolean, equalityMap: EqualityMap): ApplicationResult = {
    equalityMap.truthValueOf(value) match {
      case TruthValue.Bottom => Constraint.Applied(equalityMap)
      case TruthValue(concrete) =>
        if (concrete == targetTruthValue) Tautology
        else Contradiction
      case TruthValue.Top =>
        if (value.hasPins) Constraint.NoProgress
        else Tautology
    }
  }

  override def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Seq[(EqualityMap, Option[Constraint])] = (
    for {
      pin <- value.pins
      result <- equalityMap.withTruthValue(pin, targetTruthValue)
    } yield result -> None
  ).toSeq
}
