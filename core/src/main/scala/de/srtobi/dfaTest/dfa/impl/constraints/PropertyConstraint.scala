package de.srtobi.dfaTest
package dfa
package impl
package constraints

case class PropertyConstraint(base: DfValue, property: String, value: DfValue) extends Constraint {
  override def toString: String = {
    s"($base).$property = $value"
  }

  override def applyConstraint(targetTruthValue: Boolean, equalityMap: EqualityMap): Constraint.ApplicationResult = {
    ???
  }

  override def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Option[Seq[(EqualityMap, Option[Constraint])]] = {
    ???
  }
}
