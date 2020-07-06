package de.srtobi.dfaTest
package dfa
package impl2

import de.srtobi.dfaTest.dfa.impl2.Value._

sealed abstract class Value {
  var block: Option[Block] = None
  var index: Int = -1
  var isBlockCondition: Boolean = false

  var state: State = Inactive

  final def activate()(implicit activator: Activator): Boolean = {
    state match {
      case Inactive =>
        state = Enqueued
        val activateBlock = block match {
          case Some(block) => block.condition.activate()
          case None => true
        }
        if (activateInner() && activateBlock) {
          if (isBlockCondition && !evaluate()(new Evaluator {}).truthValue.isConcrete) {
            state = Enqueued
            activator.needsGuess(this)
            false
          } else {
            state = Ready
            activator.done(this)
            true
          }
        } else {
          assert(state == Enqueued)
          activator.enqueue(this)
          false
        }
      case Enqueued =>
        false
      case _ :Done | Ready =>
        true
    }
  }

  protected def activateInner()(implicit activator: Activator): Boolean

  final def evaluate()(implicit evaluator: Evaluator): DfAbstractAny = {
    state match {
      case Unreachable => return DfNothing
      case Done(result) => return result
      case Inactive | Enqueued => throw new IllegalStateException
      case Ready =>
    }

    block match {
      case Some(Block(condition, targetTruthValue)) if !condition.evaluate().truthValue.canBe(targetTruthValue) =>
        state = Unreachable
        DfNothing

      case _ =>
        val result = evaluateInner()
        state = Done(result)
        result
    }
  }

  protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny

  final def activateAll(first: Value, rest: Value*)(implicit activator: Activator): Boolean = {
    val firstA = first.activate()
    activateAll(rest) && firstA
  }

  final def activateAll(values: IterableOnce[Value])(implicit activator: Activator): Boolean = {
    var allActivated = true
    for (value <- values.iterator) {
      val result = value.activate()
      allActivated &&= result
    }
    allActivated
  }

  final def allTrue(bools: Boolean*): Boolean =
    bools.reduce(_ && _)
}

object Value {
  sealed trait State
  case object Inactive extends State
  case object Enqueued extends State
  case object Ready extends State
  case object Unreachable extends State
  case class Done(result: DfAbstractAny) extends State

  abstract class Evaluator {
  }

  abstract class Activator {
    def enqueue(value: Value): Unit
    def needsGuess(value: Value): Unit
    def done(value: Value): Unit
  }
}

sealed abstract class Operation extends Value {

}

final class UnknownValue(val name: String) extends Operation {
  override protected def activateInner()(implicit activator: Activator): Boolean = true

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = {
    DfAny
  }
}

final class Constant(val value: DfAbstractAny) extends Operation {
  override def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)
    true
  }
  override def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = value
}

final class UnifyOperation(val sources: Set[Value]) extends Operation {

  override def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)
    activateAll(sources)
  }

  override def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny =
    DfValue.unify(sources.iterator.map(_.evaluate()))
}

final class EqualityOperation(val left: Value, val right: Value) extends Operation {
  override def activateInner()(implicit activator: Activator): Boolean =
    activateAll(left, right)

  override def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = {
    val left = this.left.evaluate()
    val right = this.right.evaluate()
    if (left.isConcrete && left == right) {
      DfTrue
    } else if (left intersects right) {
      DfBoolean
    } else {
      DfFalse
    }
  }
}

final class InvertOperation(val source: Value) extends Operation {
  override protected def activateInner()(implicit activator: Activator): Boolean =
    source.activate()

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = {
    source.evaluate().truthValue.invert.toDfValue
  }
}

final class TruthyOperation(val source: Value) extends Operation {
  override protected def activateInner()(implicit activator: Activator): Boolean =
    source.activate()

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny =
    source.evaluate().truthValue.toDfValue
}

final class WritePropertyOperation(val base: Value,
                                   val property: String,
                                   val input: Value,
                                   val previousWrite: Set[WritePropertyOperation]) extends Operation {
  override protected def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)
    allTrue(
      activateAll(base, input),
      activateAll(previousWrite)
    )
  }

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = DfTrue

  def resolve(obj: DfAbstractAny)(implicit evaluator: Evaluator): DfAbstractAny = {
    val myState = evaluate()
    assert(myState == DfTrue || myState == DfNothing)

    if (myState == DfNothing) {
      return DfNothing
    }

    def allPrevious = DfValue.unify(previousWrite.iterator.map(_.resolve(obj)))
    def input = this.input.evaluate()
    def all = input unify allPrevious
    lazy val base = this.base.evaluate()

    obj match {
      case DfNothing => DfNothing
      case obj: DfConcreteAnyRef =>
        if (obj == base) input
        else if (obj <= base) all
        else allPrevious
      case _ if obj intersects base => all
      case _ => allPrevious
    }
  }
}

final class ReadPropertyOperation(val base: Value,
                                  val property: String,
                                  val previousWrite: Set[WritePropertyOperation]) extends Operation {
  override protected def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)
    allTrue(
      activateAll(base),
      activateAll(previousWrite)
    )
  }

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = {
    val obj = base.evaluate()
    DfValue.unify(
      previousWrite.iterator
        .map(_.resolve(obj))
    )
  }
}

final class Summary(val values: Seq[Value], val previousWrite: Map[String, Set[WritePropertyOperation]], localScope: DfConcreteObjectRef) extends Operation {
  override def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)

    allTrue(
      activateAll(values),
      activateAll(previousWrite.valuesIterator.flatten)
    )
  }

  override def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = DfNothing

  def summarize()(implicit evaluator: Evaluator): Map[DfConcreteObjectRef, Map[String, DfAbstractAny]] = {
    evaluate()

    val result = Map.newBuilder[DfConcreteObjectRef, Map[String, DfAbstractAny]]

    explore(localScope) {
      (visit, cur) =>
        val props =
          previousWrite
            .view
            .mapValues(_.foldLeft(DfNothing: DfAbstractAny)(_ unify _.resolve(cur)))
            .filterNot(_._2.isNothing)
            .toMap
        result += cur ->props

        for (x <- props.valuesIterator; obj <- x.concreteObjectRefs)
          visit(obj)
    }

    result.result()
  }
}