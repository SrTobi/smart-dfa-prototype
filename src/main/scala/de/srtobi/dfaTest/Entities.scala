package de.srtobi.dfaTest

import de.srtobi.dfaTest.cfg.ControlFlowGraph


sealed trait DfEntity

sealed abstract class DfVariable extends DfEntity {
  def name: String

  override def toString: String = name
}

class DfRegister(val id: Int) extends DfVariable {
  override def name: String = "%" + Integer.toUnsignedString(id, 36)
}

case class DfLocalVariable(override val name: String) extends DfVariable


sealed trait DfAbstractValue

abstract class DfInternalAbstractValue extends DfAbstractValue

sealed trait DfAbstractAny extends DfAbstractValue {
  def canBeAllOf(value: DfAbstractAny): Boolean
}

case object DfAny extends DfAbstractAny {
  override def canBeAllOf(value: DfAbstractAny): Boolean = value == DfAny
}

sealed abstract class DfConcreteAny extends DfAbstractAny with DfEntity {
  final override def canBeAllOf(value: DfAbstractAny): Boolean = this == value
}

sealed abstract class DfConcreteAnyRef extends DfConcreteAny

class DfConcreteObjectRef extends DfConcreteAnyRef

class DfConcreteStringRef(value: String) extends DfConcreteAnyRef {
  override def toString: String = "\"%s\"".format(value)
}

class DfConcreteLambdaRef(val lambda: Ast.Function,
                          val params: Seq[DfConcreteLambdaRef.Parameter],
                          val cfg: ControlFlowGraph) extends DfConcreteAnyRef {
  override def toString: String =
    s"lambda(${params.mkString(", ")})"
}

case class DfConcreteInternalFunc(name: String) extends DfConcreteAnyRef {
  override def toString: String =
    s"internalFunc[$name]"
}

object DfConcreteLambdaRef {
  class Parameter(val variable: DfVariable) {
    def name: String = variable.name
    override def toString: String = name
  }
}

sealed abstract class DfConcreteAnyVal extends DfConcreteAny {
  type Type <: AnyVal
  def value: Type
}

case object DfUndefined extends DfConcreteAnyVal {
  override type Type = Unit
  override def value: Type = ()

  override def toString: String = "undefined"
}

sealed trait DfAbstractBoolean extends DfAbstractAny {
  def unify(other: DfAbstractBoolean): DfAbstractBoolean
  def negative: DfAbstractBoolean
}

case object DfBoolean extends DfAbstractBoolean {
  override def canBeAllOf(value: DfAbstractAny): Boolean = value.isInstanceOf[DfAbstractBoolean]
  override def unify(other: DfAbstractBoolean): DfBoolean.type = DfBoolean
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
  override def negative: DfTrue.type = DfTrue

  override def toString: String = "false"
}

sealed trait DfAbstractInt extends DfAbstractAny {
  def unify(other: DfAbstractInt): DfAbstractInt
}

case object DfInt extends DfAbstractInt {
  override def canBeAllOf(value: DfAbstractAny): Boolean = value.isInstanceOf[DfAbstractInt]
  override def unify(other: DfAbstractInt): DfInt.type = DfInt
}

case class DfConcreteInt(override val value: Int) extends DfConcreteAnyVal with DfAbstractInt {
  override type Type = Int
  override def toString: String = value.toString
  override def unify(other: DfAbstractInt): DfAbstractInt = other match {
    case DfConcreteInt(`value`) => this
    case _ => DfInt
  }
}

object DfValue {
  def int: DfInt.type = DfInt
  def int(value: Int): DfConcreteInt = DfConcreteInt(value)

  def boolean: DfBoolean.type = DfBoolean
  def boolean(value: Boolean): DfConcreteBoolean = DfConcreteBoolean(value)

  def undefined: DfUndefined.type = DfUndefined

  def unify(values: IterableOnce[DfAbstractAny]): DfAbstractAny = {
    var any = false
    var undef: DfUndefined.type = null
    var bool: DfAbstractBoolean = null
    var int: DfAbstractInt = null
    val setBuilder = Set.newBuilder[DfAbstractAny]

    def add(values: IterableOnce[DfAbstractAny]): Unit = values.iterator.takeWhile(_ => !any).foreach {
      case DfAny => any = true
      case DfUndefined => undef = DfUndefined
      case abstractBool: DfAbstractBoolean => bool = bool unify abstractBool
      case abstractInt: DfAbstractInt => int = int unify abstractInt
      case ref: DfConcreteAnyRef => setBuilder += ref
      case DfUnionType(values) => add(values)
    }

    add(values)
    if (any) return DfAny
    setBuilder ++= Option(undef)
    setBuilder ++= Option(bool)
    setBuilder ++= Option(int)

    val set = setBuilder.result()
    set.size match {
      case 0 => ???
      case 1 => set.head
      case _ => DfUnionType(set)
    }
  }

  private case class DfUnionType(values: Set[DfAbstractAny]) extends DfAbstractAny {
    assert(values.size >= 2)
    assert(!values.exists(_.isInstanceOf[DfUnionType]))

    override def canBeAllOf(value: DfAbstractAny): Boolean = ???
  }
}
