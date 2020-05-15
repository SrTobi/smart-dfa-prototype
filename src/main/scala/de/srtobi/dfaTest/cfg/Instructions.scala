package de.srtobi.dfaTest
package cfg

import de.srtobi.dfaTest.dfa._

sealed abstract class Instruction {
  private var _index: Int = -1
  private var _graph: ControlFlowGraph = _
  private var _labels: Set[Label] = _

  def index: Int = {
    assert(_index >= 0)
    _index
  }

  def lineNumber: Int= index + 1

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

  def nextInstruction: Instruction = {
    assert(info.hasControlFlowAfter)
    graph.instructionAt(index + 1)
  }

  def sourceEntities: Seq[DfVarOrValue]
  def variables: Seq[DfVariable]
  def entities: Seq[DfVarOrValue] = sourceEntities ++ variables

  def asmString: String
  def asmLine: String = s"$lineNumber: $asmString"
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
    case reg: DfRegister => reg.toString + " <- "
    case variable => variable.toString + " = "
  }
}


//*********************************** JumpingInstruction ***********************************//
sealed abstract class JumpingInstruction extends Instruction {
  def targetLabel: Label
}

object JumpingInstruction {
  object to {
    def unapply(jumpingInstruction: JumpingInstruction): Some[Label] = Some(jumpingInstruction.targetLabel)
  }
}


//*********************************** End ***********************************//
final class End private[cfg] extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq.empty
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = "end"
  override def info: Instruction.Info = End
}

object End extends Instruction.Info(
  name = "End",
  hasControlFlowAfter = false
)


//*********************************** Noop ***********************************//
final class Noop private[cfg](val value: DfVarOrValue) extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq(value)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"noop $value"
  override def info: Instruction.Info = Noop
}

object Noop extends Instruction.Info("Noop") {
  def unapply(noop: Noop): Some[DfVarOrValue] = Some(noop.value)
}


//*********************************** Ret ***********************************//
final class Ret private[cfg](val returnValue: DfVarOrValue) extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq(returnValue)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"ret $returnValue"
  override def info: Instruction.Info = Ret
}

object Ret extends Instruction.Info(
  name = "Ret",
  hasControlFlowAfter = false
) {
  def unapply(ret: Ret): Some[DfVarOrValue] = Some(ret.returnValue)
}

//*********************************** Call ***********************************//
final class Call private[cfg](val target: Option[DfVariable],
                              val func: DfVarOrValue,
                              val args: Seq[DfVarOrValue]) extends Instruction {

  override def sourceEntities: Seq[DfVarOrValue] = args
  override def variables: Seq[DfVariable] = target.toSeq

  override def asmString: String = {
    val builder = new StringBuilder

    target.foreach { ret =>
      builder.append(Instruction.asmAssignmentPrefix(ret))
    }

    builder.append("call [")
    builder.append(func)
    builder.append("]")

    builder.append(args.mkString("(", ", ", ")"))

    builder.toString()
  }

  override def info: Instruction.Info = Call
}

object Call extends Instruction.Info("Call") {
  def unapply(call: Call): Some[(Option[DfVariable], DfVarOrValue, Seq[DfVarOrValue])] = Some((call.target, call.func, call.args))
}


//*********************************** BinaryOp ***********************************//
final class BinaryOp private[cfg](val target: DfVariable, val left: DfVarOrValue, val op: String, val right: DfVarOrValue) extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq(left, right)
  override def variables: Seq[DfVariable] = Seq(target)
  override def asmString: String = Instruction.asmAssignmentPrefix(target) + s"$left $op $right"
  override def info: Instruction.Info = BinaryOp
}

object BinaryOp extends Instruction.Info("BinaryOp") {
  def unapply(binaryOp: BinaryOp): Some[(DfVariable, DfVarOrValue, String, DfVarOrValue)] = Some((binaryOp.target, binaryOp.left, binaryOp.op, binaryOp.right))
}


//*********************************** Jump ***********************************//
/**
 * Continues control flow at the target instruction
 *
 * @param targetLabel to the instruction where the control flow should be continued
 */
final class Jump private[cfg](override val targetLabel: Label) extends JumpingInstruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq.empty
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"jmp $targetLabel"
  override def info: Instruction.Info = Jump
}

object Jump extends Instruction.Info(
  name = "Jump",
  hasControlFlowAfter = false,
  isJump = true
) {
  def unapply(jump: Jump): Some[Label] = Some(jump.targetLabel)
}


//*********************************** JumpIfNot ***********************************//
final class JumpIfNot private[cfg](val condition: DfVarOrValue, override val targetLabel: Label) extends JumpingInstruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq(condition)
  override def variables: Seq[DfVariable] = Seq.empty
  override def asmString: String = s"ifNot $condition -> $targetLabel"
  override def info: Instruction.Info = JumpIfNot
}

object JumpIfNot extends Instruction.Info(
  name = "JumpIfNot",
  hasControlFlowAfter = true,
  isJump = true
) {
  def unapply(jumpIfNot: JumpIfNot): Some[(DfVarOrValue, Label)] = Some((jumpIfNot.condition, jumpIfNot.targetLabel))
}


//*********************************** Mov ***********************************//
final class Mov private[cfg](val target: DfVariable, val source: DfVarOrValue) extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq(source)
  override def variables: Seq[DfVariable] = Seq(target)
  override def asmString: String = Instruction.asmAssignmentPrefix(target) + source
  override def info: Instruction.Info = Mov
}

object Mov extends Instruction.Info("Mov") {
  def unapply(mov: Mov): Some[(DfVariable, DfVarOrValue)] = Some((mov.target, mov.source))
}


//*********************************** New ***********************************//
final class New private[cfg](val target: DfVariable) extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq.empty
  override def variables: Seq[DfVariable] = Seq(target)
  override def asmString: String = Instruction.asmAssignmentPrefix(target) + s"new"
  override def info: Instruction.Info = New
}

object New extends Instruction.Info("New") {
  def unapply(n: New): Some[DfVariable] = Some(n.target)
}


//*********************************** ReadProp ***********************************//
final class ReadProp private[cfg](val target: DfVariable, val base: DfVarOrValue, val member: String) extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq()
  override def variables: Seq[DfVariable] = Seq(target)
  override def asmString: String = s"$target <- [$base].$member"
  override def info: Instruction.Info = ReadProp
}

object ReadProp extends Instruction.Info("ReadProp") {
  def unapply(readProp: ReadProp): Some[(DfVariable, DfVarOrValue, String)] = Some((readProp.target, readProp.base, readProp.member))
}


//*********************************** WriteProp ***********************************//
final class WriteProp private[cfg](val base: DfVarOrValue, val member: String, val value: DfVarOrValue) extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq(value)
  override def variables: Seq[DfVariable] = Seq()
  override def asmString: String = s"[$base].$member = $value"
  override def info: Instruction.Info = WriteProp
}

object WriteProp extends Instruction.Info("WriteProp") {
  def unapply(writeProp: WriteProp): Some[(DfVarOrValue, String, DfVarOrValue)] = Some((writeProp.base, writeProp.member, writeProp.value))
}


//*********************************** Debug ***********************************//
final class Debug private[cfg](val checks: Seq[Debug.Check]) extends Instruction {
  override def sourceEntities: Seq[DfVarOrValue] = Seq()
  override def variables: Seq[DfVariable] = Seq()
  override def asmString: String = s"debug [${checks.mkString(", ")}]"
  override def info: Instruction.Info = Debug
}

object Debug extends Instruction.Info("Debug") {
  def unapply(debug: Debug): Some[Seq[Check]] = Some(debug.checks)

  sealed abstract class Check
  case object CheckDeadCode extends Check {
    override def toString: String = "dead"
  }
  case object CheckLiveCode extends Check {
    override def toString: String = "live"
  }

  sealed trait Expectation
  object Expectation {
    case class VarOrValue(varOrValue: DfVarOrValue) extends Expectation {
      override def toString: String = varOrValue.toString
    }
    case class Value(value: DfAbstractAny) extends Expectation {
      override def toString: String = value.toString
    }
    case class SubclassOf(clazz: Class[_ <: DfAbstractAny]) extends Expectation {
      override def toString: String = clazz.getSimpleName
    }

    private val expectationByName = Map(
      "any" -> SubclassOf(classOf[DfAbstractAny]),
      "abs_any" -> Value(DfAny),
      "con_any" -> SubclassOf(classOf[DfConcreteAny]),

      "con_anyval" -> SubclassOf(classOf[DfConcreteAnyVal]),

      "bool" -> SubclassOf(classOf[DfAbstractBoolean]),
      "abs_bool" -> Value(DfBoolean),
      "con_bool" -> SubclassOf(classOf[DfConcreteBoolean]),

      "int" -> SubclassOf(classOf[DfAbstractInt]),
      "abs_int" -> Value(DfInt),
      "con_int" -> SubclassOf(classOf[DfConcreteInt]),

      "string" -> SubclassOf(classOf[DfConcreteStringRef])
    )

    def fromPropertyName(name: String): Option[Expectation] =
      expectationByName.get(name)
  }
  case class Is(actual: DfVarOrValue, expected: Expectation, exprText: String) extends Check {
    override def toString: String = s"$actual is $expected"
  }
  case class Print(entity: DfVarOrValue, exprText: String) extends Check {
    override def toString: String = s"print $entity"
  }
}