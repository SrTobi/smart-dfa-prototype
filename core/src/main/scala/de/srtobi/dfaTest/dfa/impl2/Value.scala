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
  var asPrecondition = Option.empty[Value]
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

  def pushState(): Unit = ()
  def popState(): Unit = ()

  def toText: String
}


sealed abstract class Value extends Operation {
  private[this] var _evaluated = Option.empty[DfAbstractAny]
  private[this] var _propagateAssumptions = Option.empty[Boolean]
  private[this] var _filter = Option.empty[DfAbstractAny]

  def evaluated: DfAbstractAny =
    if (!active) DfNothing
    else {
      val evaled = _evaluated.get
      _filter.fold(evaled)(_ intersect evaled)
    }

  def setPropagateAssumptions(value: Boolean): Unit = _propagateAssumptions = Some(value)

  def isPropagatingAssumptions: Boolean = _propagateAssumptions.isDefined

  protected[this] def evaluated_=(value: DfAbstractAny): Unit =
    _evaluated = Some(value)

  protected[this] def propagateAssumptions(targetTruth: Boolean): Unit = ()

  override def process(): Unit = {
    super.process()
    if (active) {
      _propagateAssumptions.foreach(propagateAssumptions)
    }
  }

  private[this] val savedEvaluated = mutable.Stack.empty[(Option[DfAbstractAny], Option[Boolean], Option[DfAbstractAny])]

  override def pushState(): Unit =
    savedEvaluated.push((_evaluated, _propagateAssumptions, _filter))

  override def popState(): Unit = {
    val (evaluated, propagateAssumptions, filter) = savedEvaluated.pop()
    _evaluated = evaluated
    _propagateAssumptions = propagateAssumptions
    _filter = filter
  }

  def setFilter(filterValue: DfAbstractAny): Unit = {
    val finalFilterValue = _filter.fold(filterValue)(_ unify filterValue)
    _filter = Some(finalFilterValue)
    propagateFilter(finalFilterValue)
  }

  protected[this] def propagateFilter(filterValue: DfAbstractAny): Unit = ()
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
  override protected def processInner(): Unit = {
    evaluated = DfAny
  }

  override def toString: String = "@" + name

  override def toText: String = s"make $this"
}

final class Constant[T >: DfNothing.type <: DfAbstractAny](val value: T) extends Value {
  override protected def processInner(): Unit = {
    evaluated = value
  }

  override def toText: String = s"$this <- $value"
}

final class UnifyOperation(val sources: Set[Value]) extends Value {
  override protected def processInner(): Unit = {
    evaluated = DfValue.unify(sources.map(_.evaluated))
  }

  override def toText: String = s"$this <- {${sources.mkString(" | ")}}"

  override protected[this] def propagateFilter(filterValue: DfAbstractAny): Unit =
    sources.foreach(_.setFilter(filterValue))
}

final class EqualityOperation(val left: Value, val right: Value) extends Value {
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

  override protected[this] def propagateFilter(filterValue: DfAbstractAny): Unit =
    if (filterValue.truthValue == TruthValue.True) {
      val intersection = left.evaluated intersect right.evaluated
      left.setFilter(intersection)
      right.setFilter(intersection)
    }
}

final class InvertOperation(val source: Value) extends Value {
  override protected def processInner(): Unit = {
    evaluated = source.evaluated.truthValue.invert.toDfValue
  }

  override def toText: String = s"$this <- !$source"

  override protected[this] def propagateFilter(filterValue: DfAbstractAny): Unit =
    filterValue.truthValue
      .asConcrete
      .map(b => DfConcreteBoolean(!b))
      .foreach(source.setFilter)
}

final class TruthyOperation(val source: Value) extends Value {
  override protected def processInner(): Unit = {
    evaluated = source.evaluated.truthValue.toDfValue
  }

  override def toText: String = s"$this <- truthy $source"

  override protected[this] def propagateFilter(filterValue: DfAbstractAny): Unit =
    source.setFilter(filterValue)
}

trait PropertyMemorySource {
  def resolve(obj: DfAbstractAny): DfAbstractAny

  def propagateFilter(obj: DfConcreteAnyRef, filter: DfAbstractAny): Unit
}



class PropertyInit extends Operation with PropertyMemorySource {
  override def resolve(obj: DfAbstractAny): DfAbstractAny = DfUndefined

  override protected def processInner(): Unit = {
    // nothing to do
  }

  override def toText: String = s"$this <- prop-init"

  override def propagateFilter(obj: DfConcreteAnyRef, filter: DfAbstractAny): Unit = ()
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
    def activeSources: Iterator[PropertyMemorySource] =
      if (active) {
        val isTrue: ((Block, PropertyMemorySource)) => Option[PropertyMemorySource] = {
          case (block, op) if block.condition.evaluated.truthValue.canBe(block.targetTruthValue) => Some(op)
          case _ => None
        }
        previousWrites.iterator.flatMap(isTrue)
      } else Iterator.empty

    def toText: String = s"$this: *.$property <- ${previousWrites.map(w => w._2.toString + " if " + w._1).mkString(" | ")}"

    override def resolve(obj: DfAbstractAny): DfAbstractAny =
      DfValue.unify(activeSources.map(_.resolve(obj)))

    override def index: Int = PropertyPhiOperation.this.index

    override def propagateFilter(obj: DfConcreteAnyRef, filter: DfAbstractAny): Unit = {
      val actives = activeSources
      if (actives.hasNext) {
        val single = actives.next()
        if (!actives.hasNext) {
          single.propagateFilter(obj, filter)
        }
      }
    }
  }
}

final class WritePropertyOperation(val base: Value,
                                   val property: String,
                                   val input: Value,
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

  override def propagateFilter(obj: DfConcreteAnyRef, filter: DfAbstractAny): Unit =
    this.base.evaluated match {
      case ref: DfConcreteAnyRef => input.setFilter(filter)
      case _ =>
    }
}

final class ReadPropertyOperation(val base: Value,
                                  val property: String,
                                  val previousWrite: PropertyMemorySource) extends Value {
  override protected def processInner(): Unit = {
    val obj = base.evaluated
    evaluated = previousWrite.resolve(obj)
  }

  override def toText: String = s"$this <- $base.$property [$previousWrite]"

  override protected[this] def propagateFilter(filterValue: DfAbstractAny): Unit = {
    this.base.evaluated match {
      case ref: DfConcreteAnyRef => previousWrite.propagateFilter(ref, filterValue)
      case _ =>
    }
  }
}

final class Summary(val values: Seq[Value], val previousWrite: Map[String, PropertyMemorySource], localScope: DfConcreteObjectRef) extends Operation {

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

final class Call(val func: Value, args: Seq[Value]) extends Value {
  private val inlined = mutable.Map.empty[DfConcreteLambdaRef, Array[Operation]]

  override protected def processInner(): Unit = {
    val f = func.evaluated

    evaluated = DfValue.unify(f.concreteRefs.collect {
      case ref: DfConcreteLambdaRef => ???
      case DfConcreteInternalFunc("rand") => DfAny
    })
  }

  override def toText: String = s"$this <- $func(${args.mkString(", ")})"
}