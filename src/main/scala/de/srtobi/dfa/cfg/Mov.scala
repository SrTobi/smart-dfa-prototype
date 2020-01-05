package de.srtobi.dfa
package cfg


class Mov private[cfg](val target: DfVariable, val source: DfEntity) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq(source)
  override def variables: Seq[DfVariable] = Seq(target)

  override def asmString: String = Instruction.asmAssignmentPrefix(target) + source

  override def info: Instruction.Info = Mov
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitMov(this)
}

object Mov extends Instruction.Info(name = "Mov")
