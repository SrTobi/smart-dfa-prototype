package de.srtobi.dfa
package cfg

class Noop private[cfg](val value: DfEntity) extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq(value)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"noop $value"
  override def info: Instruction.Info = Noop
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitNoop(this)
}

object Noop extends Instruction.Info(
  name = "Noop",
  hasControlFlowAfter = false
)
