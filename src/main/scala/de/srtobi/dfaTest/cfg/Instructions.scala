package de.srtobi.dfaTest
package cfg

sealed abstract class Instruction {
  private var _index: Int = -1
  private var _graph: ControlFlowGraph = _
  private var _labels: Set[Label] = _

  def index: Int = {
    assert(_index >= 0)
    _index
  }

  private[dfaTest] def index_=(idx: Int): Unit = {
    assert(_index == -1)
    assert(idx >= 0)
    _index = idx
  }

  def graph: ControlFlowGraph = {
    assert(_graph != null)
    _graph
  }

  def labels: Set[Label] = {
    assert(_labels != null)
    _labels
  }

  def sourceEntities: Seq[DfEntity]
  def variables: Seq[DfVariable]
  def entities: Seq[DfEntity] = sourceEntities ++ variables

  def asmString: String
  def asmLine: String = s"$index: $asmString"
  def info: Instruction.Info

  override def toString: String = asmString
}


object Instruction {

  case class Info(name: String,
                  hasControlFlowAfter: Boolean = true,
                  isJump: Boolean = false)

  private[dfaTest] def finalizeInstruction(instr: Instruction, graph: ControlFlowGraph, labels: Set[Label]): Unit = {
    assert(instr._index >= 0)
    assert(instr._graph == null)
    assert(instr._labels == null)

    instr._graph = graph
    instr._labels = labels
  }

  def asmAssignmentPrefix(v: DfVariable): String = v match {
    case reg: DfRegister => reg + " <- "
    case variable => variable + " = "
  }
}


//*********************************** JumpingInstruction ***********************************//
sealed abstract class JumpingInstruction extends Instruction {
  def targetLabel: Label
}

object JumpingInstruction {
  object to {
    def unapply(jumpingInstruction: JumpingInstruction): Option[Label] = Some(jumpingInstruction.targetLabel)
  }
}


//*********************************** End ***********************************//
final class End private[cfg] extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = "end"
  override def info: Instruction.Info = End
}

object End extends Instruction.Info(
  name = "End",
  hasControlFlowAfter = false
) {
  def unapply(end: End): Boolean = true
}


//*********************************** Noop ***********************************//
final class Noop private[cfg](val value: DfEntity) extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq(value)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"noop $value"
  override def info: Instruction.Info = Noop
}

object Noop extends Instruction.Info("Noop") {
  def unapply(noop: Noop): Option[DfEntity] = Some(noop.value)
}


//*********************************** Ret ***********************************//
final class Ret private[cfg](val returnValue: DfEntity) extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq(returnValue)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"ret $returnValue"
  override def info: Instruction.Info = Ret
}

object Ret extends Instruction.Info(
  name = "Ret",
  hasControlFlowAfter = false
) {
  def unapply(ret: Ret): Option[DfEntity] = Some(ret.returnValue)
}

//*********************************** Call ***********************************//
final class Call private[cfg](val target: Option[DfVariable],
                        val funcEntity: DfEntity,
                        val args: Seq[DfEntity]) extends Instruction {


  override def sourceEntities: Seq[DfEntity] = args
  override def variables: Seq[DfVariable] = target.toSeq

  override def asmString: String = {
    val builder = new StringBuilder

    target.foreach { ret =>
      builder.append(Instruction.asmAssignmentPrefix(ret))
    }

    builder.append("call [")
    builder.append(funcEntity)
    builder.append("]")

    builder.append(args.mkString("(", ", ", ")"))

    builder.toString()
  }

  override def info: Instruction.Info = Call
}

object Call extends Instruction.Info("Call") {
  def unapply(call: Call): Option[(Option[DfVariable], DfEntity, Seq[DfEntity])] = Some((call.target, call.funcEntity, call.args))
}


//*********************************** BinaryOp ***********************************//
final class BinaryOp private[cfg](val target: DfVariable, val left: DfEntity, val op: String, val right: DfEntity) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq(left, right)
  override def variables: Seq[DfVariable] = Seq(target)

  override def asmString: String = Instruction.asmAssignmentPrefix(target) + s"$left $op $right"

  override def info: Instruction.Info = BinaryOp
}

object BinaryOp extends Instruction.Info("BinaryOp") {
  def unapply(binaryOp: BinaryOp): Option[(DfVariable, DfEntity, String, DfEntity)] = Some((binaryOp.target, binaryOp.left, binaryOp.op, binaryOp.right))
}


//*********************************** Jump ***********************************//
/**
 * Continues control flow at the target instruction
 *
 * @param targetLabel to the instruction where the control flow should be continued
 */
final class Jump private[cfg](override val targetLabel: Label) extends JumpingInstruction {

  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"jmp $targetLabel"
  override def info: Instruction.Info = Jump
}

object Jump extends Instruction.Info(
  name = "Jump",
  hasControlFlowAfter = false,
  isJump = true
) {
  def unapply(jump: Jump): Option[Label] = Some(jump.targetLabel)
}


//*********************************** JumpIfNot ***********************************//
final class JumpIfNot private[cfg](val condition: DfEntity, val targetLabel: Label) extends JumpingInstruction {
  override def sourceEntities: Seq[DfEntity] = Seq(condition)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"ifNot $condition -> $targetLabel"
  override def info: Instruction.Info = JumpIfNot
}

object JumpIfNot extends Instruction.Info(
  name = "JumpIfNot",
  hasControlFlowAfter = true,
  isJump = true
) {
  def unapply(jumpIfNot: JumpIfNot): Option[(DfEntity, Label)] = Some((jumpIfNot.condition, jumpIfNot.targetLabel))
}


//*********************************** Mov ***********************************//
final class Mov private[cfg](val target: DfVariable, val source: DfEntity) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq(source)
  override def variables: Seq[DfVariable] = Seq(target)

  override def asmString: String = Instruction.asmAssignmentPrefix(target) + source

  override def info: Instruction.Info = Mov
}

object Mov extends Instruction.Info("Mov") {
  def unapply(mov: Mov): Option[(DfVariable, DfEntity)] = Some((mov.target, mov.source))
}


//*********************************** New ***********************************//
final class New private[cfg](val target: DfVariable) extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq(target)

  override def asmString: String = Instruction.asmAssignmentPrefix(target) + s"new"

  override def info: Instruction.Info = New
}

object New extends Instruction.Info("New") {
  def unapply(n: New): Option[DfVariable] = Some(n.target)
}


//*********************************** ReadProp ***********************************//
final class ReadProp private[cfg](val target: DfVariable, val base: DfEntity, val member: String) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq()
  override def variables: Seq[DfVariable] = Seq(target)
  override def asmString: String = s"$target <- [$base].$member"
  override def info: Instruction.Info = ReadProp
}

object ReadProp extends Instruction.Info("ReadProp") {
  def unapply(readProp: ReadProp): Option[(DfVariable, DfEntity, String)] = Some((readProp.target, readProp.base, readProp.member))
}


//*********************************** WriteProp ***********************************//
final class WriteProp private[cfg](val base: DfEntity, val member: String, val value: DfEntity) extends Instruction {

  override def sourceEntities: Seq[DfEntity] = Seq(value)
  override def variables: Seq[DfVariable] = Seq()
  override def asmString: String = s"[$base].$member = $value"
  override def info: Instruction.Info = WriteProp
}

object WriteProp extends Instruction.Info("WriteProp") {
  def unapply(writeProp: WriteProp): Option[(DfEntity, String, DfEntity)] = Some((writeProp.base, writeProp.member, writeProp.value))
}
