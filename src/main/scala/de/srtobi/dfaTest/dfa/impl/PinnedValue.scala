package de.srtobi.dfaTest
package dfa
package impl

sealed abstract class AnalysisEntity extends DfExternalEntity[ExecutionState]

final case class PinnedValue(anchor: Any)(val name: String) extends AnalysisEntity {
  override def normalize(context: ExecutionState): DfAbstractAny = context.normalize(this)
  override def toString(context: ExecutionState): String = {
    val normal = normalize(context).toString()
    s"$normal[$name]"
  }
}

final case class UnionValue(value: DfAbstractAny, pinnedValues: Set[PinnedValue]) extends AnalysisEntity {
  assert(pinnedValues.size + (if (value.isNothing) 0 else 1) >= 2)

  def values: Iterator[DfPinned] = {
    value.withoutNothing.iterator ++ pinnedValues.iterator.map(DfPinned.apply)
  }

  override def normalize(context: ExecutionState): DfAbstractAny =
    dfa.DfValue.unify(Iterator(value) ++ pinnedValues.map(context.normalize))

  override def toString(context: ExecutionState): String = {
    val normal = normalize(context).toString()
    val strs = value.withoutNothing.map(_.toString()) ++ pinnedValues.map(_.name)
    s"$normal[${strs.mkString(" + ")}]"
  }
}
