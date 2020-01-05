package de.srtobi.dfa
package cfg

class JumpIfNot private[cfg](val condition: DfEntity, val targetLabel: Label) extends JumpingInstruction {
  override def sourceEntities: Seq[DfEntity] = Seq(condition)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"ifNot $condition -> $targetLabel"
  override def info: Instruction.Info = JumpIfNot
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitJumpIf(this)
}

object JumpIfNot extends Instruction.Info(
  name = "JumpIfNot",
  hasControlFlowAfter = true,
  isJump = true
)
