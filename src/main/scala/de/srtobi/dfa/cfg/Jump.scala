package de.srtobi.dfa
package cfg

/**
 * Continues control flow at the target instruction
 *
 * @param targetLabel to the instruction where the control flow should be continued
 */
class Jump private[cfg](override val targetLabel: Label) extends JumpingInstruction {

  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"jmp $targetLabel"
  override def info: Instruction.Info = Jump
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitJump(this)
}

object Jump extends Instruction.Info(
  name = "Jump",
  hasControlFlowAfter = false,
  isJump = true
)
