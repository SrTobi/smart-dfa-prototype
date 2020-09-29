package de.srtobi.dfaTest.dfa

trait Unifiable[T] {
  def unify(entities: IterableOnce[T]): T
}

object Unifiable {
  def unify[T: Unifiable](first: T, rest: T*): T = unify(first +: rest)
  def unify[T: Unifiable](entities: IterableOnce[T]): T = implicitly[Unifiable[T]].unify(entities)
}


object Test {
  trait JoinLattice[A] {
    def join(first: A, second: A): A
  }

  implicit class JoinLatticeExt[A](val lattice: A) extends AnyVal {
    def join(other: A)(implicit joiner: JoinLattice[A]): A = joiner.join(lattice, other)
  }


  abstract class BoolLat {

  }

  implicit val BoolLatJoin: JoinLattice[BoolLat] =  ???

  abstract class SemiBoolLat extends BoolLat {

  }

  implicit val SemiBoolLatJoin: JoinLattice[SemiBoolLat] = ???


  val b = new BoolLat {}
  val sb = new SemiBoolLat {}

}

/*
object Test2 {

  trait JoinSemiLattice[T <: JoinSemiLattice[T, LB], LB <: JoinSemiLattice[T, LB]] { this: LB =>

    def join[Other >: LB <: T](other: Other): Other

    def joinAll[Other >: LB <: T](others: IterableOnce[Other]): Other =
      others.iterator.foldLeft(this: Other)(_ join _)
  }

  class BoolLat extends JoinSemiLattice[BoolLat, SemiBoolLat] {
    override def join[Other >: SemiBoolLat <: BoolLat](other: Other): Other = new BoolLat
  }

  class SemiBoolLat extends JoinSemiLattice[BoolLat, SemiBoolLat] {
    override def join[Other >: SemiBoolLat <: BoolLat](other: Other): Other = ???
  }
}


 */