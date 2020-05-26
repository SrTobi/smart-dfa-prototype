package de.srtobi.dfaTest.dfa

trait Unifiable[T] {
  def unify(entities: IterableOnce[T]): T
}

object Unifiable {
  def unify[T: Unifiable](first: T, rest: T*): T = unify(first +: rest)
  def unify[T: Unifiable](entities: IterableOnce[T]): T = implicitly[Unifiable[T]].unify(entities)
}