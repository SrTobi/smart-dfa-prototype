package de.srtobi.dfa
package cfg

class Call private[cfg](val ret: Option[DfVariable],
                        val funcEntity: DfEntity,
                        val args: Seq[DfEntity]) extends Instruction {


  override def sourceEntities: Seq[DfEntity] = args
  override def variables: Seq[DfVariable] = ret.toSeq

  override def asmString: String = {
    val builder = new StringBuilder

    ret.foreach { ret =>
      builder.append(Instruction.asmAssignmentPrefix(ret))
    }

    builder.append("call [")
    builder.append(funcEntity)
    builder.append("]")

    builder.append(args.mkString("(", ", ", ")"))

    builder.toString()
  }

  override def info: Instruction.Info = Jump
  override def accept(visitor: InstructionVisitor): Unit = visitor.visitCall(this)
}

object Call extends Instruction.Info(
  name = "Call",
  hasControlFlowAfter = true
)
