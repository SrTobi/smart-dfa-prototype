package de.srtobi.dfaTest
package dfa
package impl

case class ExecutionState(variables: Map[DfVariable, DfValue]) {
  def withStore(variable: DfVariable, value: DfValue): ExecutionState =
    this.copy(variables = variables + (variable -> value))


  def unify(other: ExecutionState): ExecutionState = ???
}

object ExecutionState {
  implicit val unifiable: Unifiable[ExecutionState] = (entities: IterableOnce[ExecutionState]) => ???
}
