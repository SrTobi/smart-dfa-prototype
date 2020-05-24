package de.srtobi.dfaTest
package dfa
package impl
package constraints

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint._

case class ConditionalConstraint(target: PinnedValue, value: DfValue, condition: Constraint) extends Constraint {
  override def toString: String = {
    s"($target = $value) if $condition"
  }

  override def applyConstraint(targetTruthValue: Boolean, equalityMap: EqualityMap): ApplicationResult = {
    assert(targetTruthValue)

    // test inner constraint
    condition.applyConstraint(targetTruthValue, equalityMap) match {
      case Tautology =>
        TransformProgress(equalityMap, EqualityConstraint(DfPinned(target), value))
      case Contradiction =>
        equalityMap.withEquality(DfPinned(target), DfNothing) match {
          case Some(equalityMap) => Applied(equalityMap)
          case None => Contradiction
        }
      case _ =>
        equalityMap.equalityKnowledge(target, value) match {
          case TruthValue.True => TransformProgress(equalityMap, condition)
          case TruthValue.False => TransformProgress(equalityMap, NotConstraint(condition))
          case _ => NoProgress
        }
    }
  }

  override def possibleGuesses(targetTruthValue: Boolean, equalityMap: EqualityMap): Seq[(EqualityMap, Option[Constraint])] = {
    assert(targetTruthValue)

    val falseIt =
      equalityMap.withEquality(DfPinned(target), DfNothing)
        .map(_ -> Some(NotConstraint(condition)))
        .iterator

    val equalityConstraint = EqualityConstraint(DfPinned(target), value)
    val trueIt =
      Iterator(equalityMap -> Some(AndConstraint(equalityConstraint, condition)))

    (trueIt ++ falseIt).toSeq
  }
}