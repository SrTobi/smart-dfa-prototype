package de.srtobi.dfaTest.dfa

import de.srtobi.dfaTest.Ast
import de.srtobi.dfaTest.cfg.ControlFlowGraph


sealed trait DfVarOrValue

sealed abstract class DfVariable extends DfVarOrValue {
  def name: String

  override def toString: String = name
}

class DfRegister(val id: Int) extends DfVariable {
  override def name: String = "%" + Integer.toUnsignedString(id, 36)
}

case class DfLocalVariable(override val name: String) extends DfVariable


sealed trait DfValue[-Context, +ExternalEntity] {
  def normalize(context: Context): DfAbstractAny
  def toString(context: Context): String
}

trait DfExternalEntity[-Context] {
  def normalize(context: Context): DfAbstractAny
  def toString(context: Context): String
}

final case class DfExternalValue[-Context, +ExternalEntity <: DfExternalEntity[Context]](entity: ExternalEntity) extends DfValue[Context, ExternalEntity] {
  override def normalize(context: Context): DfAbstractAny = entity.normalize(context)
  override def toString(context: Context): String = entity.toString(context)

  override def toString: String = entity.toString
}

sealed trait DfAbstractAny extends DfValue[Any, Nothing] {
  final def isNothing: Boolean = this == DfNothing
  final def withoutNothing: Option[DfAbstractAny] =
    if (isNothing) None else Some(this)
  final def unify(other: DfAbstractAny): DfAbstractAny = DfValue.unify(this, other)

  final override def normalize(context: Any): this.type = this
  final override def toString(context: Any): String = this.toString()

  def <=(other: DfAbstractAny): Boolean = DfValue.intersect(this, other) == this
  def intersect(other: DfAbstractAny): DfAbstractAny = DfValue.intersect(this, other)
  def intersects(other: DfAbstractAny): Boolean = (this intersect other) != DfNothing
  def canBeAllOf(value: DfAbstractAny): Boolean
  def truthValue: TruthValue
  def isConcrete: Boolean = false
  def concreteObjectRefs: Iterator[DfConcreteObjectRef] = this match {
    case obj: DfConcreteObjectRef => Iterator(obj)
    case DfAbstractUnion(values) => values.iterator.flatMap(_.concreteObjectRefs)
    case _ => Iterator.empty
  }
}

object DfAbstractAny {
  implicit val unifiable: Unifiable[DfAbstractAny] =
    (entities: IterableOnce[DfAbstractAny]) => DfValue.unify(entities)
}

case object DfNothing extends DfAbstractAny with DfAbstractBoolean {
  override def truthValue: TruthValue = TruthValue.Bottom

  override def canBeAllOf(value: DfAbstractAny): Boolean = false

  override def unify(other: DfAbstractBoolean): DfAbstractBoolean = other
  override def couldBe(bool: Boolean): false = false
  override def negative: DfNothing.type = DfNothing
}

case object DfAny extends DfAbstractAny {
  override def canBeAllOf(value: DfAbstractAny): Boolean = value == DfAny
  override def truthValue: TruthValue = TruthValue.Top
}

sealed abstract class DfConcreteAny extends DfAbstractAny with DfVarOrValue {
  final override def canBeAllOf(value: DfAbstractAny): Boolean = this == value
  override def isConcrete: Boolean = true
}

sealed abstract class DfConcreteAnyRef extends DfConcreteAny

class DfConcreteObjectRef(name: String) extends DfConcreteAnyRef {
  override def toString: String = s"{$name}"
  override def truthValue: TruthValue = TruthValue.True
}

class DfConcreteLambdaRef(val lambda: Ast.Function,
                          val params: Seq[DfConcreteLambdaRef.Parameter],
                          val cfg: ControlFlowGraph) extends DfConcreteAnyRef {
  override def toString: String =
    s"lambda(${params.mkString(", ")})"

  override def truthValue: TruthValue = TruthValue.True
}

case class DfConcreteInternalFunc(name: String) extends DfConcreteAnyRef {
  override def toString: String =
    s"internalFunc[$name]"

  override def truthValue: TruthValue = TruthValue.True
}

object DfConcreteLambdaRef {
  class Parameter(val variable: DfVariable) {
    def name: String = variable.name
    override def toString: String = name
  }
}

sealed abstract class DfConcreteAnyVal extends DfConcreteAny {
  type Type
  def value: Type
}

case object DfUndefined extends DfConcreteAnyVal {
  override type Type = Unit
  override def value: Type = ()

  override def truthValue: TruthValue = TruthValue.False

  override def toString: String = "undefined"
}

sealed trait DfAbstractBoolean extends DfAbstractAny {
  def unify(other: DfAbstractBoolean): DfAbstractBoolean
  def couldBe(bool: Boolean): Boolean
  def negative: DfAbstractBoolean
}

object DfAbstractBoolean {
  implicit val unifiable: Unifiable[DfAbstractBoolean] =
    (entities: IterableOnce[DfAbstractBoolean]) => {
      val it = entities.iterator
      var acc = it.next()
      while (it.hasNext && acc != DfBoolean) {
        acc = acc unify it.next()
      }
      acc
    }
}

case object DfBoolean extends DfAbstractBoolean {
  override def canBeAllOf(value: DfAbstractAny): Boolean = value.isInstanceOf[DfAbstractBoolean]
  override def truthValue: TruthValue = TruthValue.Top
  override def unify(other: DfAbstractBoolean): DfBoolean.type = DfBoolean
  override def couldBe(bool: Boolean): Boolean = true
  override def negative: DfBoolean.type = DfBoolean
}

sealed abstract class DfConcreteBoolean extends DfConcreteAnyVal with DfAbstractBoolean {
  override type Type <: Boolean
  override def value: Type
}

object DfConcreteBoolean {
  def apply(value: Boolean): DfConcreteBoolean =
    if (value) DfTrue else DfFalse

  def unapply(arg: DfConcreteBoolean): Some[Boolean] = Some(arg.value)
}

case object DfTrue extends DfConcreteBoolean {
  override type Type = true
  override def value: Type = true

  override def unify(other: DfAbstractBoolean): DfAbstractBoolean = other match {
    case DfTrue => DfTrue
    case _ => DfBoolean
  }
  override def couldBe(bool: Boolean): Boolean = bool
  override def truthValue: TruthValue = TruthValue.True
  override def negative: DfFalse.type = DfFalse

  override def toString: String = "true"
}

case object DfFalse extends DfConcreteBoolean {
  override type Type = false
  override def value: Type = false

  override def unify(other: DfAbstractBoolean): DfAbstractBoolean = other match {
    case DfFalse => DfFalse
    case _ => DfBoolean
  }
  override def couldBe(bool: Boolean): Boolean = !bool
  override def truthValue: TruthValue = TruthValue.False
  override def negative: DfTrue.type = DfTrue

  override def toString: String = "false"
}

sealed trait DfAbstractInt extends DfAbstractAny {
  def unify(other: DfAbstractInt): DfAbstractInt
}

case object DfInt extends DfAbstractInt {
  override def canBeAllOf(value: DfAbstractAny): Boolean = value.isInstanceOf[DfAbstractInt]
  override def truthValue: TruthValue = TruthValue.Top
  override def unify(other: DfAbstractInt): DfInt.type = DfInt
}

final case class DfConcreteInt(override val value: Int) extends DfConcreteAnyVal with DfAbstractInt {
  override type Type = Int
  override def toString: String = value.toString
  override def truthValue: TruthValue = TruthValue(value != 0)
  override def unify(other: DfAbstractInt): DfAbstractInt = other match {
    case DfConcreteInt(`value`) => this
    case _ => DfInt
  }
}

case class DfConcreteString(override val value: String) extends DfConcreteAnyVal {
  override type Type = String

  override def toString: String = "\"%s\"".format(value)

  override def truthValue: TruthValue = TruthValue(value != "")
}

case class DfAbstractUnion(values: Set[DfAbstractAny]) extends DfAbstractAny {
  assert(values.size >= 2)
  assert(!values.exists(_.isInstanceOf[DfAbstractUnion]))
  assert(!values.contains(DfNothing))

  override def truthValue: TruthValue = TruthValue.unifiable.unify(values.iterator.map(_.truthValue))
  override def canBeAllOf(value: DfAbstractAny): Boolean = ???

  override def toString: String = values.mkString("Union[", " | ", "]")
}

object DfValue {
  def nothing: DfNothing.type  = DfNothing
  def int: DfInt.type = DfInt
  def int(value: Int): DfConcreteInt = DfConcreteInt(value)

  def boolean: DfBoolean.type = DfBoolean
  def boolean(value: Boolean): DfConcreteBoolean = DfConcreteBoolean(value)

  def undefined: DfUndefined.type = DfUndefined

  val negativeValue: DfAbstractAny = unify(undefined, int(0), DfFalse)

  def unify(first: DfAbstractAny, rest: DfAbstractAny*): DfAbstractAny = unify(first +: rest)
  def unify(values: IterableOnce[DfAbstractAny]): DfAbstractAny = {
    var any = false
    var undef: DfUndefined.type = null
    var bool = Option.empty[DfAbstractBoolean]
    var int = Option.empty[DfAbstractInt]
    val setBuilder = Set.newBuilder[DfAbstractAny]

    def add(values: IterableOnce[DfAbstractAny]): Unit = values.iterator.takeWhile(_ => !any).foreach {
      case DfNothing =>
      case DfAny => any = true
      case DfUndefined => undef = DfUndefined
      case abstractBool: DfAbstractBoolean => bool = bool.map(_ unify abstractBool).orElse(Some(abstractBool))
      case abstractInt: DfAbstractInt => int = int.map(_ unify abstractInt).orElse(Some(abstractInt))
      case string: DfConcreteString => setBuilder += string
      case ref: DfConcreteAnyRef => setBuilder += ref
      case DfAbstractUnion(values) => add(values)
    }

    add(values)
    if (any) return DfAny
    setBuilder ++= Option(undef)
    setBuilder ++= bool
    setBuilder ++= int

    val set = setBuilder.result()
    set.size match {
      case 0 => DfNothing
      case 1 => set.head
      case _ => DfAbstractUnion(set)
    }
  }

  def intersect(first: DfAbstractAny, rest: DfAbstractAny*): DfAbstractAny = intersect(first +: rest)

  def intersect(a: IterableOnce[DfAbstractAny]): DfAbstractAny = {
    def toSet(maybeUnion: DfAbstractAny): Set[DfAbstractAny] = maybeUnion match {
      case DfAbstractUnion(values) => values
      case DfNothing => Set.empty
      case _ => Set(maybeUnion)
    }

    def combine(a: Set[DfAbstractAny], b: Set[DfAbstractAny]): Set[DfAbstractAny] = {
      if (a.isEmpty) a
      else if (b.isEmpty) b
      else if (a.contains(DfAny)) b
      else if (b.contains(DfAny)) a
      else {
        val result = Set.newBuilder[DfAbstractAny]
        for (e <- a) e match {
          case DfBoolean => result ++= b.find(_.isInstanceOf[DfAbstractBoolean])
          case DfInt => result ++= b.find(_.isInstanceOf[DfAbstractInt])
          case bool: DfConcreteBoolean if b.contains(DfBoolean) => result += bool
          case int: DfConcreteInt if b.contains(DfInt) => result += int
          case concrete if b.contains(concrete) =>
            result += concrete
          case _ =>
        }
        result.result()
      }
    }

    a.iterator.map(toSet).reduceOption(combine).fold(DfAny: DfAbstractAny) {
      set =>
        set.size match {
          case 0 => DfNothing
          case 1 => set.head
          case _ => DfAbstractUnion(set)
        }
    }
  }
}
