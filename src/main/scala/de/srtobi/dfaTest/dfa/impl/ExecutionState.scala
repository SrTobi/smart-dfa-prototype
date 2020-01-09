package de.srtobi.dfaTest
package dfa
package impl

case class ExecutionState(variables: Map[DfVariable, DfValue]) extends Unifiable {
  override type UnifiableRoot = ExecutionState

  def withStore(variable: DfVariable, value: DfValue): ExecutionState =
    this.copy(variables = variables + (variable -> value))


  override def unify(entities: IterableOnce[ExecutionState]): ExecutionState = ???
}
