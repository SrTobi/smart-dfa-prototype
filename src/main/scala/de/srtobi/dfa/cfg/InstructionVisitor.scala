package de.srtobi.dfa.cfg

abstract class InstructionVisitor {
  def visitInstruction(instruction: Instruction): Unit = ()

  def visitMov(mov: Mov): Unit = visitInstruction(mov)
  def visitWrite(write: Write): Unit = visitInstruction(write)
  def visitWrite(writeProp: WriteProp): Unit = visitInstruction(writeProp)
  def visitRead(read: Read): Unit = visitInstruction(read)
  def visitReadProp(readProp: ReadProp): Unit = visitInstruction(readProp)
  def visitBinaryOp(binaryOp: BinaryOp): Unit = visitInstruction(binaryOp)
  def visitCall(call: Call): Unit = visitInstruction(call)
  def visitNew(newInstr: New): Unit = visitInstruction(newInstr)
  def visitJump(jump: Jump): Unit = visitInstruction(jump)
  def visitJumpIfNot(jumpIf: JumpIfNot): Unit = visitInstruction(jumpIf)
  def visitRet(ret: Ret): Unit = visitInstruction(ret)
  def visitEnd(end: End): Unit = visitInstruction(end)
  def visitNoop(noop: Noop): Unit = visitInstruction(noop)
}

