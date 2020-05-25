package de.srtobi.dfaTest
package dfa

import scala.collection.mutable

package object impl {
  type DfValue = dfa.DfValue[ExecutionState, AnalysisEntity]

  object DfValue {
    def unify(first: DfValue, rest: DfValue*): DfValue =
      unify(first +: rest)

    def unify(values: IterableOnce[DfValue]): DfValue = {
      var abstracts = mutable.Buffer.empty[DfAbstractAny]
      val pinned = Set.newBuilder[PinnedValue]

      values.iterator.foreach {
        case DfAE(pin: PinnedValue) => pinned += pin
        case DfAE(union: UnionValue) =>
          abstracts += union.value
          pinned ++= union.pinnedValues
        case abs: DfAbstractAny =>
          abstracts += abs
      }

      val abs = dfa.DfValue.unify(abstracts)
      val pinneds = pinned.result()

      if (pinneds.isEmpty) abs
      else if (pinneds.size >= 2 || abs != DfNothing) DfAE(UnionValue(abs, pinneds))
      else DfAE(pinneds.head)
    }
  }

  implicit class DfValueOps(private val dfValue: DfValue) extends AnyVal {
    def unify(other: DfValue*): DfValue = DfValue.unify(dfValue +: other)

    def toDfPinned: Option[DfPinned] = dfValue match {
      case DfAE(pin: PinnedValue) => Some(DfPinned(pin))
      case any: DfAbstractAny => Some(any)
      case _ => None
    }

    def dfPinnedValues: Iterator[DfPinned] = dfValue match {
      case DfAE(union: UnionValue) => union.values
      case DfAE(other: PinnedValue) => Iterator(DfPinned(other))
      case abs: DfAbstractAny => Iterator(abs)
    }

    def pins: Iterator[PinnedValue] = dfValue match {
      case DfAE(union: UnionValue) => union.pinnedValues.iterator
      case DfAE(other: PinnedValue) => Iterator(other)
      case _: DfAbstractAny => Iterator.empty
    }

    def deconstruct: (DfAbstractAny, Iterator[PinnedValue]) = dfValue match {
      case DfAE(UnionValue(any, pinnedValues)) => (any, pinnedValues.iterator)
      case DfAE(other: PinnedValue) => (DfNothing, Iterator(other))
      case any: DfAbstractAny => (any, Iterator.empty)
    }

    def hasPins: Boolean = !dfValue.isInstanceOf[DfAbstractAny]
  }

  type DfPinned = dfa.DfValue[ExecutionState, PinnedValue]
  object DfPinned {
    def apply(entity: PinnedValue): DfPinned = DfExternalValue[ExecutionState, PinnedValue](entity)
    def fromAnchor(any: Any, name: String): DfPinned = DfPinned(PinnedValue(any)(name))

    def unapply(value: dfa.DfExternalValue[ExecutionState, PinnedValue]): Some[PinnedValue] = Some(value.entity)
  }

  type DfAE = DfExternalValue[ExecutionState, AnalysisEntity]
  object DfAE {
    def apply(ae: AnalysisEntity): DfAE = DfExternalValue(ae)
    def unapply(ev: DfExternalValue[ExecutionState, AnalysisEntity]): Some[AnalysisEntity] = Some(ev.entity)
  }
}
