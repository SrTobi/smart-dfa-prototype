package de.srtobi.dfaTest
package dfa
package impl

sealed abstract class AnalysisEntity extends DfExternalEntity {
  override type Context = ExecutionState

  override def normalize(context: ExecutionState): DfAbstractAny = ???
}
