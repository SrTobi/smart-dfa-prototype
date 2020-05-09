package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint.ConstraintDemand

trait Constraint {
  def result: PinnedValue
  //def mkString(arguments: Seq[DfValue]): String
  //def evaluate(arguments: Seq[DfValue]): DfValue

  def propagate(targetTruthValue: Boolean): IterableOnce[Seq[ConstraintDemand]]
}

object Constraint {
  sealed trait ConstraintDemand
  case class TruthyDemand(value: DfValue) extends ConstraintDemand
  case class EqualityDemand(a: DfValue, b: DfValue) extends ConstraintDemand
  case class InvertedDemand(inner: ConstraintDemand) extends ConstraintDemand

  object InvertedDemand {
    def invertIf(cond: Boolean)(inner: ConstraintDemand): ConstraintDemand =
      if (cond) InvertedDemand(inner) else inner
  }
}