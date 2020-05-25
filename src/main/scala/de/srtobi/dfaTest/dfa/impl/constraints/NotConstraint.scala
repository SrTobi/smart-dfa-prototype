package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint.ApplicationResult

case class NotConstraint(inner: Constraint) extends Constraint {
  override def toString: String = {
    s"!$inner"
  }

  override def applyConstraint(targetTruthValue: Boolean, equalityMap: EqualityMap): ApplicationResult =
    inner.applyConstraint(!targetTruthValue, equalityMap)
  override def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Option[Seq[(EqualityMap, Option[Constraint])]] =
    inner.possibleGuesses(!targetTruthValue, equalityMap)
}
