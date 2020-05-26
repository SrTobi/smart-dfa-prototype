package de.srtobi.dfaTest
package dfa
package impl


case class EqualityMap private(var parents: Map[PinnedValue, Either[PinnedValue, Info]]) {
  def truthValueOf(value: DfValue): TruthValue = Unifiable.unify(
    value.dfPinnedValues.map {
      case any: DfAbstractAny => any.truthValue
      case DfPinned(pin) => findInfo(pin).truthValue
    }
  )

  def isNothing(value: DfValue): Boolean = value match {
    case DfAE(pin: PinnedValue) => isNothing(pin)
    case DfAE(UnionValue(any, pins)) => any.isNothing && pins.forall(isNothing)
    case any: DfAbstractAny => any.isNothing
  }

  def isNothing(pin: PinnedValue): Boolean =
    findInfo(pin).isNothing

  def normalize(value: DfValue): DfAbstractAny = Unifiable.unify(
    value.dfPinnedValues.map {
      case any: DfAbstractAny => any
      case DfPinned(pin) => abstractValueOf(pin)
    }
  )

  def abstractValueOf(pin: PinnedValue): DfAbstractAny =
    findInfo(pin).value

  def concreteValues: Map[PinnedValue, DfAbstractAny] = {
    parents.keysIterator
      .map(k => k -> abstractValueOf(k))
      .toMap
  }

  def isEqual(pin: PinnedValue, other: DfValue): TruthValue = {
    if (other == DfNothing)
      return TruthValue.Bottom

    val info = findInfo(pin)

    other match {
      case _ if info.isNothing => TruthValue.Bottom
      case DfPinned(pin) => isEqual(findInfo(pin), info)
      case any: DfAbstractAny =>
        val normalizedOther = normalize(other)
        if (normalizedOther intersects any) TruthValue.Top
        else TruthValue.False
    }
  }

  def withTruthValue(pin: PinnedValue, truthValue: Boolean): Option[EqualityMap] = {
    val info = findInfo(pin)
    val newTruthValue = info.truthValue intersect TruthValue(truthValue)
    val newValue = newTruthValue match {
      case TruthValue.Bottom => DfNothing
      case TruthValue.False => info.value intersect dfa.DfValue.negativeValue
      case _ => info.value
    }

    val newInfo = info.copy(truthValue = newTruthValue, value = newValue)
    copy(parents + (info.proxy -> Right(newInfo))).checked()
  }

  def withUpperBound(value: DfValue, upperBound: DfAbstractAny): Option[EqualityMap] = {
    val (any, pins) = value.deconstruct

    if (!(any <= upperBound)) {
      return None
    }

    var result = this

    for(proxy <- pins.map(findProxy).distinct) {
      val info = infoOf(proxy)
      val intersection = info.value intersect upperBound
      if (intersection != info.value) {
        val newInfo = info.copy(
          truthValue = info.truthValue intersect intersection.truthValue,
          value = intersection,
        )
        result = result.copy(
          result.parents + (proxy -> Right(newInfo))
        )
      }
    }

    result.checked()
  }

  def withEquality(a: DfPinned, b: DfPinned, equal: Boolean = true): Option[EqualityMap] = {
    (a, b) match {
      case (DfPinned(a), DfPinned(b)) => withEquality(findInfo(a), findInfo(b), equal)
      case (DfPinned(pin), any: DfAbstractAny) => withEquality(findInfo(pin), any, equal)
      case (any: DfAbstractAny, DfPinned(pin)) => withEquality(findInfo(pin), any, equal)
      case (a: DfAbstractAny, b: DfAbstractAny) =>
        if (a intersects b) Some(this)
        else None
    }
  }

  private def withEquality(a: Info, b: Info, equal: Boolean): Option[EqualityMap] = {
    if (isEqual(a, b) == TruthValue(equal))
      return Some(this)

    if (equal) {
      // merge
      val bInequalities = b.inequalities.filter(isProxy)
      val intersection = a.value intersect b.value
      val merged = a.copy(
        truthValue = if (intersection.isNothing) TruthValue.Bottom else a.truthValue intersect b.truthValue,
        inequalities = a.inequalities | bInequalities,
        value = intersection
      )
      val updatedParents = bInequalities.iterator
        .foldLeft(parents) {
          case (parents, pin) =>
            val otherInfo = parents(pin)
              .getOrElse(throw new AssertionError("should be a proxy!"))
            val updatedInfo = otherInfo.copy(
              inequalities = otherInfo.inequalities + a.proxy
            )
            parents + (pin -> Right(updatedInfo))
        }
      copy(updatedParents + (b.proxy -> Left(a.proxy)) + (a.proxy -> Right(merged))).checked()
    } else {
      copy(parents +
        (a.proxy -> Right(a.copy(inequalities = a.inequalities + b.proxy))) +
        (b.proxy -> Right(b.copy(inequalities = b.inequalities + a.proxy)))
      ).checked()
    }
  }

  private def withEquality(info: EqualityMap.Info, any: DfAbstractAny, bool: Boolean): Option[EqualityMap] = {
    if (bool) {
      val intersection = info.value intersect any
      if (intersection == info.value) {
        Some(this)
      } else {
        val newInfo = info.copy(
          truthValue = info.truthValue intersect intersection.truthValue,
          value = intersection,
        )

        copy(parents + (info.proxy -> Right(newInfo))).checked()
      }
    } else {
      // todo: do something?
      Some(this)
    }
  }

  private def checked(): Option[this.type] = Some(this)

  private def isEqual(a: Info, b: Info): TruthValue = {
    if (a.isNothing || b.isNothing) TruthValue.Bottom
    else if ((a eq b) || a.proxy == b.proxy) TruthValue.True
    else if (a.inequalities.contains(b.proxy)) TruthValue.False
    else if ((a.value intersects b.value) && (a.truthValue intersects b.truthValue)) TruthValue.Top
    else TruthValue.False
  }

  private def findInfo(pin: PinnedValue): Info =
    infoOf(findProxy(pin))

  private def infoOf(proxy: PinnedValue): Info = {
    parents.get(proxy) match {
      case Some(info) => info.getOrElse(throw new AssertionError(s"$proxy is supposed to be a proxy"))
      case None => Info(proxy)
    }
  }

  private def findProxy(pin: PinnedValue): PinnedValue =
    parents.get(pin) match {
      case Some(Left(parent)) =>
        val proxy = findProxy(parent)
        if (proxy != parent) {
          // union find path compression
          parents += pin -> Left(proxy)
        }
        proxy
      case _ =>
        pin
    }

  private def isProxy(pin: PinnedValue): Boolean =
    findProxy(pin) == pin
}

object EqualityMap {
  val empty = new EqualityMap(Map.empty)

  case class Info(proxy: PinnedValue,
                  truthValue: TruthValue = TruthValue.Top,
                  inequalities: Set[PinnedValue] = Set.empty,
                  value: DfAbstractAny = DfAny) {
    assert(truthValue <= value.truthValue)
    assert(inequalities.contains(proxy) ==> isNothing)

    def concrete: Option[DfConcreteAny] = value match  {
      case concrete: DfConcreteAny => Some(concrete)
      case _ => None
    }

    def isNothing: Boolean = value.isNothing

    override def equals(o: Any): Boolean = throw new AssertionError("should not be called")
  }
}
