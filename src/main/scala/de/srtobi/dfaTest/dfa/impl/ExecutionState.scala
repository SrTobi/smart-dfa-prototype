package de.srtobi.dfaTest
package dfa
package impl

case class ExecutionState(variables: Map[DfVariable, DfAbstractValue]) extends Unifiable {
  override type UnifiableRoot = ExecutionState

  def withStore(variable: DfVariable, value: DfAbstractValue): ExecutionState =
    this.copy(variables = variables + (variable -> value))


  override def unify(entities: IterableOnce[ExecutionState]): ExecutionState = ???
}
