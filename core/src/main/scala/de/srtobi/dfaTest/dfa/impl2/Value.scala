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
    val oldState = state
    state match {
      case Inactive =>
        state = Enqueued
        val activateBlock = block match {
          case Some(block) => block.condition.activate()
          case None => true
        }
        val result = if (activateInner() && activateBlock) {
          state = Ready
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

        //println(s"activate $this: $oldState -> $state")
        result
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
      case Inactive | Enqueued => throw new IllegalStateException(s"bad state in $this")
      case Ready =>
    }

    val oldState = state
    val result = block match {
      case Some(Block(condition, targetTruthValue)) if !condition.evaluate().truthValue.canBe(targetTruthValue) =>
        state = Unreachable
        DfNothing

      case _ =>
        val result = evaluateInner()
        state = Done(result)
        result
    }

    //println(s"evaluate $this: $oldState -> $state")
    result
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

  override def toString: String = "%" + index

  def toText: String
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

  override def toString: String = "@" + name

  override def toText: String = s"make $this"
}

final class Constant(val value: DfAbstractAny) extends Operation {
  override def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)
    true
  }
  override def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = value

  override def toText: String = s"$this <- $value"
}

final class UnifyOperation(val sources: Set[Value]) extends Operation {

  override def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)
    activateAll(sources)
  }

  override def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny =
    DfValue.unify(sources.iterator.map(_.evaluate()))

  override def toText: String = s"$this <- {${sources.mkString(" | ")}}"
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

  override def toText: String = s"$this <- $left == $right"
}

final class InvertOperation(val source: Value) extends Operation {
  override protected def activateInner()(implicit activator: Activator): Boolean =
    source.activate()

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = {
    source.evaluate().truthValue.invert.toDfValue
  }

  override def toText: String = s"$this <- !$source"
}

final class TruthyOperation(val source: Value) extends Operation {
  override protected def activateInner()(implicit activator: Activator): Boolean =
    source.activate()

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny =
    source.evaluate().truthValue.toDfValue

  override def toText: String = s"$this <- truthy $source"
}

abstract class PropertyMemorySource extends Operation {
  def resolve(obj: DfAbstractAny)(implicit evaluator: Evaluator): DfAbstractAny
}



class PropertyInit extends PropertyMemorySource {
  override def resolve(obj: DfAbstractAny)(implicit evaluator: Evaluator): DfAbstractAny = DfUndefined
  override protected def activateInner()(implicit activator: Activator): Boolean = true

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = DfNothing

  override def toText: String = s"$this <- prop-init"
}



final class PropertyPhiOperation(val property: String,
                                 val previousWrites: Seq[(Block, PropertyMemorySource)]) extends PropertyMemorySource {
  override protected def activateInner()(implicit activator: Activator): Boolean =
    activateAll(previousWrites.iterator.map(_._2))

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = DfNothing

  override def resolve(obj: DfAbstractAny)(implicit evaluator: Evaluator): DfAbstractAny = {
    val isTrue: ((Block, PropertyMemorySource)) => Option[PropertyMemorySource] = {
      case (block, op) if block.condition.evaluate().truthValue.canBe(block.targetTruthValue) => Some(op)
      case _ => None
    }

    DfValue.unify(previousWrites.flatMap(isTrue).map(_.resolve(obj)))
  }

  override def toText: String = s"$this: $property <- ${previousWrites.map(w => w._2 + " if " + w._1).mkString(" | ")}"
}

final class WritePropertyOperation(val base: Value,
                                   val property: String,
                                   val input: Value,
                                   val previousWrite: PropertyMemorySource) extends PropertyMemorySource {
  override protected def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)
    allTrue(
      activateAll(base, input),
      previousWrite.activate()
    )
  }

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = DfTrue

  override def resolve(obj: DfAbstractAny)(implicit evaluator: Evaluator): DfAbstractAny = {
    val myState = evaluate()
    assert(myState == DfTrue || myState == DfNothing)

    if (myState == DfNothing) {
      return DfNothing
    }
    def allPrevious = previousWrite.resolve(obj)
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

  override def toText: String = s"$this: $base.$property <- $input [$previousWrite]"
}

final class ReadPropertyOperation(val base: Value,
                                  val property: String,
                                  val previousWrite: PropertyMemorySource) extends Operation {
  override protected def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)
    allTrue(
      activateAll(base),
      activateAll(previousWrite)
    )
  }

  override protected def evaluateInner()(implicit evaluator: Evaluator): DfAbstractAny = {
    val obj = base.evaluate()
    previousWrite.resolve(obj)
  }

  override def toText: String = s"$this <- $base.$property [$previousWrite]"
}

final class Summary(val values: Seq[Value], val previousWrite: Map[String, PropertyMemorySource], localScope: DfConcreteObjectRef) extends Operation {
  override def activateInner()(implicit activator: Activator): Boolean = {
    assert(!isBlockCondition)

    allTrue(
      activateAll(values),
      activateAll(previousWrite.valuesIterator)
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
            .mapValues(_.resolve(cur))
            .filterNot(_._2.isNothing)
            .toMap
        result += cur ->props

        for (x <- props.valuesIterator; obj <- x.concreteObjectRefs)
          visit(obj)
    }

    result.result()
  }

  override def toText: String = s"summary"
}