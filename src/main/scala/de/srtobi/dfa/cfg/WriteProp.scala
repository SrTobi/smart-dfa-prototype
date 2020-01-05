package de.srtobi.dfa
package cfg

class WriteProp private[cfg](val base: DfEntity, val member: String, val value: DfEntity) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq(value)
  override def variables: Seq[DfVariable] = Seq()
  override def asmString: String = s"[$base].$member = $value"
  override def info: Instruction.Info = WriteProp
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitWrite(this)
}

object WriteProp extends Instruction.Info(name = "WriteProp")
