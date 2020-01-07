package de.srtobi.dfa
package cfg

class BinaryOp private[cfg](val target: DfVariable, val left: DfEntity, val op: String, val right: DfEntity) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq(left, right)
  override def variables: Seq[DfVariable] = Seq(target)

  override def asmString: String = Instruction.asmAssignmentPrefix(target) + s"$left $op $right"

  override def info: Instruction.Info = BinaryOp
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitBinaryOp(this)
}

object BinaryOp extends Instruction.Info(name = "BinaryOp")
