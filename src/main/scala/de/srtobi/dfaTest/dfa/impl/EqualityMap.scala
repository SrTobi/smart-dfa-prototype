package de.srtobi.dfaTest
package dfa
package impl


import de.srtobi.dfaTest.dfa.impl.EqualityMap.Proxy

case class EqualityMap private(var parents: Map[PinnedValue, Proxy],
                               inequalities: Map[PinnedValue, Set[Proxy]],
                               abstractKnowledge: Map[PinnedValue, DfAbstractAny],
                               truthyKnowledge: Map[PinnedValue, Boolean]) {
  def truthValueOf(value: DfValue): TruthValue = {
    Unifiable.unify(value.dfPinnedValues.map {
      case any: DfAbstractAny => any.truthValue
      case DfPinned(pin) => truthValueOf(pin)
    })
  }

  def normalize(value: DfValue): DfAbstractAny = Unifiable.unify(
    value.dfPinnedValues.map {
      case any: DfAbstractAny => any
      case DfPinned(pin) => findProxy(pin).fold(identity, abstractValuesOf)
    }
  )

  def equalityKnowledge(pin: PinnedValue, other: DfValue): TruthValue = {
    lazy val normalizedOther = normalize(other)
    findProxy(pin) match {
      case Left(concrete) =>
        val intersects = concrete intersects normalizedOther
        if (!intersects) TruthValue.False
        else if (normalizedOther.isConcrete) TruthValue(intersects)
        else TruthValue.Top
      case Right(pin) =>
        other match {
          case DfPinned(otherPin) if pin == otherPin => TruthValue.True
          case _ if (abstractValuesOf(pin) intersects normalizedOther) => TruthValue.Top
          case _ => TruthValue.False
        }
    }
  }

  def concreteValues: Map[PinnedValue, DfAbstractAny] = {
    (parents.keys ++ abstractKnowledge.keySet)
      .map(k => findProxy(k).fold(k -> _, k -> abstractValuesOf(_)))
      .toMap
  }

  def withTruthValue(pin: PinnedValue, truthValue: Boolean): Option[EqualityMap] = {
    findProxy(pin) match {
      case Left(abs) =>
        if (abs.truthValue.canBe(truthValue)) Some(this)
        else None
      case Right(proxy) =>
        truthyKnowledge.get(proxy) match {
          case Some(prior) =>
            if (prior == truthValue) Some(this)
            else None
          case None =>
            Some(copy(truthyKnowledge = truthyKnowledge + (proxy -> truthValue)))
        }
    }
  }

  def withEquality(a: DfPinned, b: DfPinned, equal: Boolean = true): Option[EqualityMap] = {
    (findProxy(a), findProxy(b)) match {
      case (Some(a), Some(b)) =>
        if (equal) withEquality(a, b)
        else withInequality(a, b)
      case (Some(a), None) =>
        if (equal) withEquality(a, b.asInstanceOf[DfAbstractAny])
        else withInequality(a, b.asInstanceOf[DfAbstractAny])
      case (None, Some(b)) =>
        if (equal) withEquality(b, a.asInstanceOf[DfAbstractAny])
        else withInequality(b, a.asInstanceOf[DfAbstractAny])
      case _ =>
        val absA = a.asInstanceOf[DfAbstractAny]
        val absB = b.asInstanceOf[DfAbstractAny]
        if (absA intersects absB) Some(this)
        else None
    }
  }

  private def withInequality(a: Proxy, b: Proxy): Option[EqualityMap] = {
    if (isCertainlyEqual(a, b)) None
    else {
      implicit class InequalitiesOps(val inequalities: Map[PinnedValue, Set[Proxy]]) {
        def addInequality(a: Proxy, b: Proxy): Map[PinnedValue, Set[Proxy]] = a match {
          case Right(pin) => inequalities.updatedWith(pin)(old => Some(old.fold(Set(b))(_ + b)))
          case _ => inequalities
        }
      }
      val newInequalities = inequalities.addInequality(a, b).addInequality(b, a)
      if (newInequalities eq inequalities) Some(this)
      else Some(copy(inequalities = newInequalities))
    }
  }

  private def withInequality(a: Proxy, abstr: DfAbstractAny): Option[EqualityMap] = {
    assert(!abstr.isConcrete)

    // there is nothing much to do, is there?
    // we know that abstr can be one of muiltiple values, but we don't know which exactly
    Some(this)
  }

  private def withEquality(a: Proxy, b: Proxy): Option[EqualityMap] = {
    val (pin, other) = (a, b) match {
      case (Right(pin), b) => (pin, b)
      case (a, Right(pin)) => (pin, a)
      case (Left(a), Left(b)) =>
        return (
          if (a == b) Some(this)
          else None
          )
    }
    if (isCertainlyUnequal(pin, b))
      return None

    Some(withMergedProxies(other, pin))
  }

  private def withEquality(a: Proxy, abstr: DfAbstractAny): Option[EqualityMap] = {
    assert(!abstr.isConcrete)

    a match {
      case Left(concrete) =>
        if (abstr intersects concrete) Some(this)
        else None
      case Right(pin) =>
        abstractKnowledge.get(pin) match {
          case Some(old) =>
            val intersection = old intersect abstr
            if (intersection.isNothing) None
            else Some(copy(
              abstractKnowledge = abstractKnowledge + (pin -> intersection)
            ))
          case None =>
            Some(copy(abstractKnowledge = abstractKnowledge + (pin -> abstr)))
        }


    }
  }

  private def isCertainlyEqual(a: Proxy, b: Proxy): Boolean = a == b

  private def isCertainlyUnequal(pin: PinnedValue, other: Proxy): Boolean = {
    assert(isProxy(pin))

    inequalities.get(pin).exists(_.contains(other))
  }

  private def withMergedProxies(newProxy: Proxy, other: PinnedValue): EqualityMap = {
    assert(isProxy(newProxy))
    assert(isProxy(other))
    assert(truthValueOf(newProxy) <= truthValueOf(Right(other)))
    def intersection = {
      val intersection = abstractValuesOf(newProxy) intersect abstractValuesOf(other)
      assert(!intersection.isNothing)
      intersection
    }

    copy(
      parents = parents + (other -> newProxy),
      inequalities =
        inequalities.get(other)
          .zip(newProxy.toOption)
          .fold(inequalities) {
            case (otherUnequals, newProxy) =>
              inequalities.updatedWith(newProxy) {
                case Some(old) =>
                  Some(old | otherUnequals)
                case None =>
                  Some(otherUnequals)
              }
          },
      abstractKnowledge =
        newProxy.fold(_ => abstractKnowledge, pin =>
          if (intersection == DfAny) abstractKnowledge
          else abstractKnowledge + (pin -> intersection)
        ),
      truthyKnowledge =
        newProxy.toOption
          .zip(truthyKnowledge.get(other))
          .fold(truthyKnowledge)(
            truthyKnowledge + _
          )
    )
  }

  private def abstractValuesOf(pin: PinnedValue): DfAbstractAny =
    abstractKnowledge.getOrElse(pin, DfAny)

  private def abstractValuesOf(proxy: Proxy): DfAbstractAny =
    proxy.fold(identity, abstractValuesOf)

  private def truthValueOf(proxy: PinnedValue): TruthValue = {
    assert(isProxy(proxy))
    val fromAbstractKnowledge = abstractKnowledge.get(proxy).fold(TruthValue.Top: TruthValue)(_.truthValue)
    val fromTruthyKnowledge = TruthValue(truthyKnowledge.get(proxy))
    assert(fromAbstractKnowledge overlaps fromTruthyKnowledge)
    fromAbstractKnowledge unify fromTruthyKnowledge
  }

  private def truthValueOf(proxy: Proxy): TruthValue = proxy match {
    case Left(value) => value.truthValue
    case Right(pin) => truthValueOf(pin)
  }

  private def findProxy(value: DfPinned): Option[Proxy] = value match {
    case DfPinned(pin) => Some(findProxy(pin))
    case concrete: DfConcreteAny => Some(Left(concrete))
    case _ => None
  }

  private def findProxy(pin: PinnedValue): Proxy =
    parents.get(pin) match {
      case Some(parent) =>
        val proxy = findProxy(parent)
        if (proxy != parent) {
          // union find path compression
          parents += pin -> proxy
        }
        proxy
      case None =>
        Right(pin)
    }

  private def findProxy(value: Proxy): Proxy = value match {
    case Right(pin) => findProxy(pin)
    case proxy => proxy
  }

  private def isProxy(proxy: Proxy): Boolean =
    findProxy(proxy) == proxy

  private def isProxy(pin: PinnedValue): Boolean =
    !parents.contains(pin)
}

object EqualityMap {
  private type Proxy = Either[DfConcreteAny, PinnedValue]
  val empty = new EqualityMap(Map.empty, Map.empty, Map.empty, Map.empty)

  case class Info(truthValue: TruthValue = TruthValue.Top,
                  inequalities: Set[PinnedValue] = Set.empty,
                  value: DfAbstractAny = DfAny)
}
