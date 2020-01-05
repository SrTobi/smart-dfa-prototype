package de.srtobi.dfa
package cfg

class End private[cfg] extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = "end"
  override def info: Instruction.Info = End
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitEnd(this)
}

object End extends Instruction.Info(
  name = "End",
  hasControlFlowAfter = false
)
