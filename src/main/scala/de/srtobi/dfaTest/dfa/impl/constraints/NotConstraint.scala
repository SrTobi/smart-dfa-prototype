package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint._

case class NotConstraint(override val result: PinnedValue, inner: DfValue) extends Constraint {
  override def toString: String = {
    s"!$inner"
  }

  override def propagate(targetTruthValue: Boolean): IterableOnce[Seq[ConstraintDemand]] = {
    Iterator(Seq(InvertedDemand.invertIf(targetTruthValue)(TruthyDemand(inner))))
  }
}
