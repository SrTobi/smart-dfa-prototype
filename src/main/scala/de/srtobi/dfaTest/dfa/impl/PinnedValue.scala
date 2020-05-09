package de.srtobi.dfaTest
package dfa
package impl

final case class PinnedValue(anchor: Any)(val name: String) extends DfExternalEntity[ExecutionState] {
  override def normalize(context: ExecutionState): DfAbstractAny = context.normalize(this)
  override def toString(context: ExecutionState): String = {
    val normal = context.normalize(this).toString()
    s"$normal[$name]"
  }
}
