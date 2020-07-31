package de.srtobi.dfaTest
package dfa
package impl2

import scala.collection.mutable


trait Indexed {
  def index: Int

  override def toString: String = {
    assert(index >= 0)
    "%" + index
  }
}

sealed abstract class Operation extends Indexed {
  var index: Int = -1
  var asPrecondition = Option.empty[Value[DfAbstractBoolean]]
  var precondition = Option.empty[Block]

  def active: Boolean = {
    assert(precondition != null)
    precondition.forall(_.active)
  }

  def process(): Unit =
    if (active) {
      processInner()
    }

  protected def processInner(): Unit

  def toText: String
}


sealed abstract class Value[+T >: DfNothing.type <: DfAbstractAny] extends Operation {
  private[this] var _evaluated = Option.empty[T]

  def evaluated: T =
    if (!active) DfNothing
    else _evaluated.get

  protected[this] def evaluated_=(value: T): Unit =
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

final class UnknownValue(val name: String) extends Value[DfAbstractAny] {
  override protected def processInner(): Unit = {
    evaluated = DfAny
  }

  override def toString: String = "@" + name

  override def toText: String = s"make $this"
}

final class Constant[T >: DfNothing.type <: DfAbstractAny](val value: T) extends Value[T] {
  override protected def processInner(): Unit = {
    evaluated = value
  }

  override def toText: String = s"$this <- $value"
}

final class UnifyOperation(val sources: Set[Value[DfAbstractAny]]) extends Value[DfAbstractAny] {
  override protected def processInner(): Unit = {
    evaluated = DfValue.unify(sources.map(_.evaluated))
  }

  override def toText: String = s"$this <- {${sources.mkString(" | ")}}"
}

final class EqualityOperation(val left: Value[DfAbstractAny], val right: Value[DfAbstractAny]) extends Value[DfAbstractBoolean] {
  override protected def processInner(): Unit = {
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
  }

  override def toText: String = s"$this <- $left == $right"
}

final class InvertOperation(val source: Value[DfAbstractAny]) extends Value[DfAbstractBoolean] {
  override protected def processInner(): Unit = {
    evaluated = source.evaluated.truthValue.invert.toDfValue
  }

  override def toText: String = s"$this <- !$source"
}

final class TruthyOperation(val source: Value[DfAbstractAny]) extends Value[DfAbstractBoolean] {
  override protected def processInner(): Unit = {
    evaluated = source.evaluated.truthValue.toDfValue
  }

  override def toText: String = s"$this <- truthy $source"
}

trait PropertyMemorySource {
  def resolve(obj: DfAbstractAny): DfAbstractAny
}



class PropertyInit extends Operation with PropertyMemorySource {
  override def resolve(obj: DfAbstractAny): DfAbstractAny = DfUndefined

  override protected def processInner(): Unit = {
    // nothing to do
  }

  override def toText: String = s"$this <- prop-init"
}



final class PropertyPhiOperation extends Operation {
  private val phis = mutable.Map.empty[String, PropertyPhi]

  def forProperty(property: String, previousWrites: Seq[(Block, PropertyMemorySource)]): PropertyMemorySource = {
    val phi = new PropertyPhi(property, previousWrites)
    phis += property -> phi
    phi
  }

  override protected def processInner(): Unit = ()

  override def toText: String = s"$this:\n  " + phis.valuesIterator.map(_.toText).mkString("\n  ")

  private final class PropertyPhi(val property: String, val previousWrites: Seq[(Block, PropertyMemorySource)]) extends PropertyMemorySource with Indexed {
    override def resolve(obj: DfAbstractAny): DfAbstractAny = {
      if (active) {
        val isTrue: ((Block, PropertyMemorySource)) => Option[PropertyMemorySource] = {
          case (block, op) if block.condition.evaluated.truthValue.canBe(block.targetTruthValue) => Some(op)
          case _ => None
        }

        DfValue.unify(previousWrites.flatMap(isTrue).map(_.resolve(obj)))
      } else DfNothing
    }

    def toText: String = s"$this: *.$property <- ${previousWrites.map(w => w._2.toString + " if " + w._1).mkString(" | ")}"

    override def index: Int = PropertyPhiOperation.this.index
  }
}

final class WritePropertyOperation(val base: Value[DfAbstractAny],
                                   val property: String,
                                   val input: Value[DfAbstractAny],
                                   val previousWrite: PropertyMemorySource)
  extends Operation
    with PropertyMemorySource
{
  override protected def processInner(): Unit = ()

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

final class ReadPropertyOperation(val base: Value[DfAbstractAny],
                                  val property: String,
                                  val previousWrite: PropertyMemorySource) extends Value[DfAbstractAny] {
  override protected def processInner(): Unit = {
    val obj = base.evaluated
    evaluated = previousWrite.resolve(obj)
  }

  override def toText: String = s"$this <- $base.$property [$previousWrite]"
}

final class Summary(val values: Seq[Value[DfAbstractAny]], val previousWrite: Map[String, PropertyMemorySource], localScope: DfConcreteObjectRef) extends Operation {

  override protected def processInner(): Unit = ()

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