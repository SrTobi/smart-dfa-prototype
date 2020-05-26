package de.srtobi

import de.srtobi.dfaTest.dfa.Unifiable

package object dfaTest {
  implicit class MapOps[K, V](private val map: Map[K, V]) extends AnyVal {
    def mergeWith(other: Map[K, V])(mergeF: (V, V) => V): Map[K, V] =
      (map.keySet | other.keySet).iterator.map { key =>
        key -> ((map.get(key), other.get(key)) match {
          case (Some(a), Some(b)) => mergeF(a, b)
          case (Some(a), _) => a
          case (_, Some(b)) => b
          case (_, _) => throw new AssertionError("Should not be possible")
        })
      }.toMap

    def mergeWithDefaul(other: Map[K, V], default: V)(mergeF: (V, V) => V): Map[K, V] =
      (map.keySet | other.keySet).iterator.map { key =>
        key -> ((map.get(key), other.get(key)) match {
          case (Some(a), Some(b)) => mergeF(a, b)
          case (Some(a), _) => mergeF(a, default)
          case (_, Some(b)) => mergeF(default, b)
          case (_, _) => throw new AssertionError("Should not be possible")
        })
      }.toMap

    def combineWith(other: Map[K, V])(mergeF: (V, V) => Option[V]): Map[K, V] =
      (map.keySet | other.keySet).iterator.flatMap { key =>
        ((map.get(key), other.get(key)) match {
          case (Some(a), Some(b)) => mergeF(a, b)
          case (Some(a), _) => Some(a)
          case (_, Some(b)) => Some(b)
          case (_, _) => throw new AssertionError("Should not be possible")
        }).map(key -> _)
      }.toMap
  }

  implicit class UnifiableExt[T: Unifiable](private val target: T) {
    def unify(other: T): T = Unifiable.unify(target, other)
    def unify(others: IterableOnce[T]): T = Unifiable.unify(Iterator(target) ++ others.iterator)
  }

  implicit class BooleanExt(private val target: Boolean) {
    def ==>(other: Boolean): Boolean = !target || other
  }
}
