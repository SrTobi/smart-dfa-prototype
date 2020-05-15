package de.srtobi

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
}