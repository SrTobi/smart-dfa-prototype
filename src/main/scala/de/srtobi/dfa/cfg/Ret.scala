package de.srtobi.dfa
package cfg

class Ret private[cfg](val returnValue: DfEntity) extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq(returnValue)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"ret $returnValue"
  override def info: Instruction.Info = Ret
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitRet(this)
}

object Ret extends Instruction.Info(
  name = "Ret",
  hasControlFlowAfter = false
)
