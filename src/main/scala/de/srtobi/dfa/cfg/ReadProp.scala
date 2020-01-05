package de.srtobi.dfa
package cfg

class ReadProp private[cfg](val target: DfVariable, val base: DfEntity, val member: String) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq()
  override def variables: Seq[DfVariable] = Seq(target)
  override def asmString: String = s"$target <- [$base].$member"
  override def info: Instruction.Info = ReadProp
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitReadProp(this)
}

object ReadProp extends Instruction.Info(name = "ReadProp")
