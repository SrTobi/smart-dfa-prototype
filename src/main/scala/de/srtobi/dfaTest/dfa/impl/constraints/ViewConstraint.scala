package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint._

case class ViewConstraint(target: PinnedValue, constraint: Constraint) extends Constraint {
  override def toString: String = s"($target <=> $constraint)"

  override def applyConstraint(targetTruthValue: Boolean, equalityMap: EqualityMap): ApplicationResult = {
    def notConstraint = NotConstraint(constraint)

    equalityMap.truthValueOf(DfPinned(target)) match {
      case TruthValue.True => TransformProgress(equalityMap, if (targetTruthValue) constraint else notConstraint)
      case TruthValue.False => TransformProgress(equalityMap, if (targetTruthValue) notConstraint else constraint)
      case _ =>
        val targetTruth =
          constraint.applyConstraint(targetTruthValue = true, equalityMap) match {
            case Tautology =>
              targetTruthValue
            case Contradiction =>
              !targetTruthValue
            case _ =>
              return NoProgress
          }

        equalityMap.withTruthValue(target, targetTruth)
          .fold(Contradiction: ApplicationResult)(Applied)
    }
  }

  override def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Seq[(EqualityMap, Option[Constraint])] =
    for {
      bool <- Seq(true, false)
      result <- equalityMap.withTruthValue(target, bool)
    } yield result -> Some(if (bool == targetTruthValue) constraint else NotConstraint(constraint))
}
