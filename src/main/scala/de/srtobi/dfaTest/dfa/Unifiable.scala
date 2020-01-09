package de.srtobi.dfaTest.dfa

trait Unifiable {
  type UnifiableRoot

  def unify(first: UnifiableRoot, rest: UnifiableRoot*): UnifiableRoot = unify(first +: rest)
  def unify(entities: IterableOnce[UnifiableRoot]): UnifiableRoot
}