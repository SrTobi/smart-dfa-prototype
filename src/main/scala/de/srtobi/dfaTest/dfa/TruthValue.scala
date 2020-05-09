package de.srtobi.dfaTest
package dfa

sealed abstract class TruthValue(val canBeTrue: Boolean, val canBeFalse: Boolean) {
  def canBe(bool: Boolean): Boolean =
    if (bool) canBeTrue else canBeFalse

  def overlaps(other: TruthValue): Boolean =
    canBeTrue && other.canBeTrue || canBeFalse && other.canBeFalse
}
object TruthValue {
  def apply(bool: Boolean): TruthValue =
    if (bool) True else False

  def apply(bool: Option[Boolean]): TruthValue =
    bool.fold(Unknown: TruthValue)(TruthValue(_))

  case object Unknown extends TruthValue(true, true)
  case object True extends TruthValue(true, false)
  case object False extends TruthValue(false, true)

  implicit val unifiable: Unifiable[TruthValue] =
    (entities: IterableOnce[TruthValue]) => {
      val it = entities.iterator
      assert(it.hasNext)
      it.next() match {
        case Unknown => Unknown
        case first => if (it.forall(_ == first)) first else Unknown
      }
    }
}