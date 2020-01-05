package de.srtobi.dfa
package cfg

class Write private[cfg](val variable: DfVariable, val value: DfEntity) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq(value)
  override def variables: Seq[DfVariable] = Seq(variable)
  override def asmString: String = s"$variable = $value"
  override def info: Instruction.Info = Write
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitWrite(this)
}

object Write extends Instruction.Info(name = "Write")
