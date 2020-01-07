package de.srtobi.dfa
package cfg

class New private[cfg](val target: DfVariable) extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq(target)

  override def asmString: String = Instruction.asmAssignmentPrefix(target) + s"new"

  override def info: Instruction.Info = New
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitNew(this)
}

object New extends Instruction.Info(name = "New")
