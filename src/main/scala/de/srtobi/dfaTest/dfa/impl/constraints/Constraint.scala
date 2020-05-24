package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint.ApplicationResult

trait Constraint {
  //def result: PinnedValue
  //def mkString(arguments: Seq[DfValue]): String
  //def evaluate(arguments: Seq[DfValue]): DfValue

  def applyConstraint(targetTruthValue: Boolean, equalityMap: EqualityMap): ApplicationResult
  def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Seq[(EqualityMap, Option[Constraint])]
}

object Constraint {
  sealed abstract class ApplicationResult
  case object Tautology extends ApplicationResult
  case class Applied(equalityMap: EqualityMap) extends ApplicationResult
  case class TransformProgress(equalityMap: EqualityMap, newConstraint: Constraint) extends ApplicationResult
  case object NoProgress extends ApplicationResult
  case object Contradiction extends ApplicationResult
}