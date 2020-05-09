package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint._

case class EqualityConstraint(override val result: PinnedValue, lhs: DfValue, rhs: DfValue) extends Constraint {
  override def toString: String = {
    s"$lhs == $rhs"
  }

  override def propagate(targetTruthValue: Boolean): IterableOnce[Seq[Constraint.ConstraintDemand]] = {
    Iterator(Seq(InvertedDemand.invertIf(targetTruthValue)(EqualityDemand(lhs, rhs))))
  }
}