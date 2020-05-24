package de.srtobi.dfaTest
package dfa

sealed abstract class TruthValue(val canBeTrue: Boolean, val canBeFalse: Boolean) {
  def canBe(bool: Boolean): Boolean =
    if (bool) canBeTrue else canBeFalse

  def overlaps(other: TruthValue): Boolean =
    canBeTrue && other.canBeTrue || canBeFalse && other.canBeFalse

  def <=(other: TruthValue): Boolean =
    canBeTrue ==> other.canBeTrue && canBeFalse ==> other.canBeFalse

}
object TruthValue {
  sealed trait ConcreteTruthValue extends TruthValue

  case object Top extends TruthValue(true, true)
  case object True extends TruthValue(true, false) with ConcreteTruthValue
  case object False extends TruthValue(false, true) with ConcreteTruthValue
  case object Bottom extends TruthValue(false, false)

  def apply(bool: Boolean): TruthValue =
    if (bool) True else False

  def apply(bool: Option[Boolean]): TruthValue =
    bool.fold(Top: TruthValue)(TruthValue(_))

  def unapply(concrete: ConcreteTruthValue): Some[Boolean] = Some(concrete.canBeTrue)

  implicit val unifiable: Unifiable[TruthValue] =
    (entities: IterableOnce[TruthValue]) => {
      val it = entities.iterator.filter(_ != Bottom)
      it.nextOption().fold(Bottom: TruthValue) {
        case Top => Top
        case first => if (it.forall(_ == first)) first else Top
      }
    }
}