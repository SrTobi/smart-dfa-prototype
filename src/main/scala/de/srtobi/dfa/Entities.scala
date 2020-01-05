package de.srtobi.dfa


sealed abstract class DfEntity {
}

sealed abstract class DfVariable extends DfEntity {
  def name: String

  override def toString: String = name
}

class DfRegister(val id: Int) extends DfVariable {
  override def name: String = "%" + Integer.toUnsignedString(id, 36)
}
case class DfLocalVariable(override val name: String) extends DfVariable
case class DfMemberVariable(override val name: String) extends DfVariable

sealed abstract class DfValue extends DfEntity


object DfValue {
  def any: DfValue = new DfAbstractValue

  def boolean(value: Boolean): DfValue = DfConcreteBoolean(value)
  def int(value: Int): DfValue = DfConcreteInt(value)

  def undefined: DfValue = DfUndefined
}

class DfAbstractValue extends DfValue {
  override def toString: String = s"AbstractValue"
}

sealed abstract class DfConcreteValue extends DfValue

class DfConcreteAnyRef extends DfConcreteValue

class DfConcreteStringRef(value: String) extends DfConcreteAnyRef {
  override def toString: String = '\"' + value + '\"'
}

class DfConcreteLambdaRef(val lambda: Ast.Function,
                          val params: Seq[DfConcreteLambdaRef.Parameter],
                          val cfg: ControlFlowGraph) extends DfConcreteAnyRef {
  override def toString: String =
    s"lambda(${params.mkString(", ")})"
}

object DfConcreteLambdaRef {
  class Parameter(val variable: DfVariable) {
    def name: String = variable.name
    override def toString: String = name
  }
}

sealed abstract class DfConcreteAnyVal extends DfValue {
  def value: AnyVal
}

case object DfUndefined extends DfConcreteAnyVal {
  def value: Unit = ()

  override def toString: String = "undefined"
}

sealed abstract class DfConcreteBoolean extends DfConcreteAnyVal {
  override def value: Boolean
}

object DfConcreteBoolean {
  def apply(value: Boolean): DfConcreteBoolean =
    if (value) DfTrue else DfFalse
}

case object DfTrue extends DfConcreteBoolean {
  override def value: Boolean = true

  override def toString: String = "true"
}

case object DfFalse extends DfConcreteBoolean {
  override def value: Boolean = false

  override def toString: String = "false"
}

sealed abstract class DfConcreteIntegral extends DfConcreteAnyVal

case class DfConcreteInt(override val value: Int) extends DfConcreteIntegral {
  override def toString: String = value.toString
}
