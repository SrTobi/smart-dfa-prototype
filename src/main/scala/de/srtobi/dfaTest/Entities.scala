package de.srtobi.dfaTest

import de.srtobi.dfaTest.cfg.ControlFlowGraph


sealed abstract class DfEntity

sealed abstract class DfVariable extends DfEntity {
  def name: String

  override def toString: String = name
}

class DfRegister(val id: Int) extends DfVariable {
  override def name: String = "%" + Integer.toUnsignedString(id, 36)
}
case class DfLocalVariable(override val name: String) extends DfVariable

sealed abstract class DfValue extends DfEntity


object DfValue {
  def any: DfValue = new DfAbstractValue

  def boolean(value: Boolean): DfConcreteValue = DfConcreteBoolean(value)
  def int(value: Int): DfConcreteValue = DfConcreteInt(value)

  def undefined: DfConcreteValue = DfUndefined
}

class DfAbstractValue extends DfValue {
  override def toString: String = s"AbstractValue"
}

sealed abstract class DfConcreteValue extends DfValue

sealed abstract class DfConcreteAnyRef extends DfConcreteValue

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

case class DfConcreteInternalFunc(val name: String) extends DfConcreteAnyRef {
  override def toString: String =
    s"internal[$name]"
}

object DfConcreteLambdaRef {
  class Parameter(val variable: DfVariable) {
    def name: String = variable.name
    override def toString: String = name
  }
}

sealed abstract class DfConcreteAnyVal extends DfConcreteValue {
  type Type <: AnyVal
  def value: Type
}

case object DfUndefined extends DfConcreteAnyVal {
  override type Type = Unit
  override def value: Type = ()

  override def toString: String = "undefined"
}

sealed abstract class DfConcreteBoolean extends DfConcreteAnyVal {
  override type Type <: Boolean
  override def value: Type
}

object DfConcreteBoolean {
  def apply(value: Boolean): DfConcreteBoolean =
    if (value) DfTrue else DfFalse

  def unapply(arg: DfConcreteBoolean): Option[Boolean] = Some(arg.value)
}

case object DfTrue extends DfConcreteBoolean {
  override type Type = true
  override def value: Type = true

  override def toString: String = "true"
}

case object DfFalse extends DfConcreteBoolean {
  override type Type = false
  override def value: Type = false

  override def toString: String = "false"
}

case class DfConcreteInt(override val value: Int) extends DfConcreteAnyVal {
  override type Type = Int
  override def toString: String = value.toString
}
