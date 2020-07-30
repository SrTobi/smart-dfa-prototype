package de.srtobi.dfaTest
package dfa
package impl2

import de.srtobi.dfaTest.dfa.impl2.Operation.Processor


sealed abstract class Operation {
  var index: Int = -1

  def process(index: Int, processor: Processor): Unit


  override def toString: String = {
    assert(index >= 0)
    "%" + index
  }
  def toText: String
}

object Operation {
  abstract class Processor {
    def processNext(index: Int): Unit
  }
}

sealed abstract class Value extends Operation {
  private var _evaluated = Option.empty[DfAbstractAny]

  def evaluated: DfAbstractAny = _evaluated.get

  protected def evaluated_=(value: DfAbstractAny): Unit =
    _evaluated = Some(value)
}

object Value {
  sealed trait State
  case object Inactive extends State
  case object Enqueued extends State
  case object Ready extends State
  case object Unreachable extends State
  case class Done(result: DfAbstractAny) extends State

  //abstract class Evaluator {
  //}

  //abstract class Activator {
  //  def enqueue(value: Value): Unit
  //  def needsGuess(value: Value): Unit
  //  def done(value: Value): Unit
  //}
}

final class UnknownValue(val name: String) extends Value {
  override def process(index: Int, processor: Processor): Unit = {
    evaluated = DfAny
    processor.processNext(index + 1)
  }

  override def toString: String = "@" + name

  override def toText: String = s"make $this"
}

final class Constant(val value: DfAbstractAny) extends Value {
  override def process(index: Int, processor: Processor): Unit = {
    evaluated = value
    processor.processNext(index + 1)
  }

  override def toText: String = s"$this <- $value"
}

final class UnifyOperation(val sources: Set[Value]) extends Value {
  override def process(index: Int, processor: Processor): Unit = {
    evaluated = DfValue.unify(sources.map(_.evaluated))
    processor.processNext(index + 1)
  }

  override def toText: String = s"$this <- {${sources.mkString(" | ")}}"
}

final class EqualityOperation(val left: Value, val right: Value) extends Value {
  override def process(index: Int, processor: Processor): Unit = {
    val left = this.left.evaluated
    val right = this.right.evaluated

    evaluated =
      if (left.isConcrete && left == right) {
        DfTrue
      } else if (left intersects right) {
        DfBoolean
      } else {
        DfFalse
      }

    processor.processNext(index + 1)
  }

  override def toText: String = s"$this <- $left == $right"
}

final class InvertOperation(val source: Value) extends Value {
  override def process(index: Int, processor: Processor): Unit = {
    evaluated = source.evaluated.truthValue.invert.toDfValue
    processor.processNext(index + 1)
  }

  override def toText: String = s"$this <- !$source"
}

final class TruthyOperation(val source: Value) extends Value {
  override def process(index: Int, processor: Processor): Unit = {
    evaluated = source.evaluated.truthValue.toDfValue
    processor.processNext(index + 1)
  }

  override def toText: String = s"$this <- truthy $source"
}

abstract class PropertyMemorySource extends Operation {
  def resolve(obj: DfAbstractAny): DfAbstractAny
}



class PropertyInit extends PropertyMemorySource {
  override def resolve(obj: DfAbstractAny): DfAbstractAny = DfUndefined

  override def process(index: Int, processor: Processor): Unit = {
    processor.processNext(index + 1)
  }

  override def toText: String = s"$this <- prop-init"
}



final class PropertyPhiOperation(val property: String,
                                 val previousWrites: Seq[(Block, PropertyMemorySource)]) extends PropertyMemorySource {
  private var active = false

  override def process(index: Int, processor: Processor): Unit = {
    active = true
    processor.processNext(index + 1)
  }

  override def resolve(obj: DfAbstractAny): DfAbstractAny = {
    if (active) {
      val isTrue: ((Block, PropertyMemorySource)) => Option[PropertyMemorySource] = {
        case (block, op) if block.condition.evaluated.truthValue.canBe(block.targetTruthValue) => Some(op)
        case _ => None
      }

      DfValue.unify(previousWrites.flatMap(isTrue).map(_.resolve(obj)))
    } else DfNothing
  }

  override def toText: String = s"$this: *.$property <- ${previousWrites.map(w => w._2 + " if " + w._1).mkString(" | ")}"
}

final class WritePropertyOperation(val base: Value,
                                   val property: String,
                                   val input: Value,
                                   val previousWrite: PropertyMemorySource) extends PropertyMemorySource {
  private var active = false

  override def process(index: Int, processor: Processor): Unit = {
    active = true
    processor.processNext(index + 1)
  }

  override def resolve(obj: DfAbstractAny): DfAbstractAny = {
    if (!active) {
      return DfNothing
    }

    def allPrevious = previousWrite.resolve(obj)
    def input = this.input.evaluated
    def all = input unify allPrevious
    lazy val base = this.base.evaluated

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
                                  val previousWrite: PropertyMemorySource) extends Value {
  override def process(index: Int, processor: Processor): Unit = {
    val obj = base.evaluated
    previousWrite.resolve(obj)
  }

  override def toText: String = s"$this <- $base.$property [$previousWrite]"
}

final class Summary(val values: Seq[Value], val previousWrite: Map[String, PropertyMemorySource], localScope: DfConcreteObjectRef) extends Operation {

  override def process(index: Int, processor: Processor): Unit = ()

  def summarize(): Map[DfConcreteObjectRef, Map[String, DfAbstractAny]] = {
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