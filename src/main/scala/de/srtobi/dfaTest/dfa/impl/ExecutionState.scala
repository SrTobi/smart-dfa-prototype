package de.srtobi.dfaTest
package dfa
package impl

import de.srtobi.dfaTest.dfa.impl.constraints.Constraint

case class ExecutionState(variables: Map[DfVariable, DfValue], facts: Facts) {
  def withStore(variable: DfVariable, value: DfValue): ExecutionState =
    this.copy(variables = variables + (variable -> value))

  def withConstraint(constraint: Constraint): ExecutionState =
    this.copy(facts = facts.withConstraint(constraint))

  def resolve(entity: DfVarOrValue): DfValue = entity match {
    case v: DfVariable => variables.getOrElse(v, DfUndefined)
    case v: DfAbstractAny => v
  }

  def normalize(pin: PinnedValue): DfAbstractAny = DfAny

  def unify(other: ExecutionState): ExecutionState = {
    ExecutionState(variables.mergeWith(other.variables)(DfValue.unify(_, _)), facts unify other.facts)
  }
}

object ExecutionState {
  def empty: ExecutionState = ExecutionState(Map.empty, Facts.empty)
  def from(input: Seq[(String, DfValue)]): ExecutionState =
    input.foldLeft(empty) {
      case (state, (name, value)) => state.withStore(DfLocalVariable(name), value)
    }
  implicit val unifiable: Unifiable[ExecutionState] = (entities: IterableOnce[ExecutionState]) => ???
}
