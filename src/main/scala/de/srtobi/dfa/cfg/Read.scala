package de.srtobi.dfa
package cfg

class Read private[cfg](val target: DfRegister, val variable: DfVariable) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq(variable)
  override def variables: Seq[DfVariable] = Seq(target)
  override def asmString: String = s"$target <- $variable"
  override def info: Instruction.Info = Read
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitRead(this)
}

object Read extends Instruction.Info(name = "Read")
