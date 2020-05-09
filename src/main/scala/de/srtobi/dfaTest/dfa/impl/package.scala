package de.srtobi.dfaTest
package dfa

package object impl {
  type DfValue = dfa.DfValue[ExecutionState, PinnedValue]
  type DfPinned = DfExternalValue[ExecutionState, PinnedValue]
  object DfPinned {
    def apply(entity: PinnedValue): DfPinned = DfExternalValue[ExecutionState, PinnedValue](entity)
    def unapply(value: DfPinned): Some[PinnedValue] = Some(value.entity)

    def fromAnchor(any: Any, name: String): DfPinned = DfPinned(PinnedValue(any)(name))
  }
}
