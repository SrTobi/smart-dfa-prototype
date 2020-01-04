package de.srtobi.dfa

import scala.collection.mutable

case class UId private (num: Int, group: String) {
  def id: String = s"$group[$num]"

  override def toString: String = id

  override def equals(o: scala.Any): Boolean = o match {
    case ref: AnyRef => this eq ref
    case _ => false
  }
}

object UId {
  private val nextIdByGroups = mutable.HashMap.empty[String, Int]
  def apply(group: String): UId = {
    val cur = nextIdByGroups.getOrElseUpdate(group, 1)
    nextIdByGroups(group) = cur + 1
    new UId(cur, group)
  }
}
