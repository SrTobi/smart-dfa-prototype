package de.srtobi.dfa
package cfg

class New private[cfg](ret: DfVariable) extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq(ret)

  override def asmString: String = Instruction.asmAssignmentPrefix(ret) + s"new"

  override def info: Instruction.Info = New
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitNew(this)
}

object New extends Instruction.Info(name = "New")
