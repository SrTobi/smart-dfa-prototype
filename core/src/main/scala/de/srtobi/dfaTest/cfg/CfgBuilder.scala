package de.srtobi.dfaTest
package cfg

import scala.collection.mutable

class CfgBuilder {
  private var nextRegisterId = 0
  private val instructions = mutable.Buffer.empty[cfg.Instruction]
  private val unboundLabels = mutable.Set.empty[BuildLabel]
  private val boundLabels = mutable.Set.empty[BuildLabel]
  private val usedLabels = mutable.Set.empty[Label]
  private var numLabelsToNextInstr = 0
  private val stringLiteralCache = mutable.Map.empty[String, DfConcreteString]
  private val variableCache = mutable.Map.empty[String, DfVariable]

  private var currentIndex = Option.empty[Int]

  private def indexOfNextInstr: Int = instructions.length

  private def hasControlFlowFromPreviousInstruction: Boolean =
    instructions.lastOption.forall(_.info.hasControlFlowAfter)

  private def newRegisterId(): Int = {
    val nextId = nextRegisterId
    nextRegisterId += 1
    nextId
  }

  private def newInstr(instr: cfg.Instruction): cfg.Instruction = {
    instr.index = indexOfNextInstr
    instr.sourceIndex = currentIndex
    instructions += instr
    numLabelsToNextInstr = 0
    instr
  }

  private def newInstr(jumpingInstr: JumpingInstruction): JumpingInstruction = {
    newInstr(jumpingInstr: cfg.Instruction)
    use(jumpingInstr.targetLabel.asInstanceOf[BuildLabel])
    jumpingInstr
  }

  private def use(label: BuildLabel): Unit = {
    assert(unboundLabels.contains(label) || boundLabels.contains(label))
    usedLabels += label
  }

  def withIndex[T](index: Int)(body: => T): T = {
    val oldIndex = currentIndex
    currentIndex = Some(index)
    val result = body
    currentIndex = oldIndex
    result
  }

  def resolveVariable(name: String): DfVariable =
    variableCache.getOrElseUpdate(name, DfLocalVariable(name))

  def mov(target: DfVariable, source: DfVarOrValue): this.type = {
    if (target != source) {
      newInstr(new Mov(target, source))
    }
    this
  }

  def assign(name: String, source: DfVarOrValue): this.type =
    mov(resolveVariable(name), source)

  def pinToNewRegister(source: DfVarOrValue): DfRegister = {
    val reg = newRegister()
    mov(reg, source)
    reg
  }

  def pinToRegister(source: DfVarOrValue): DfRegister = source match {
    case reg: DfRegister => reg
    case nonReg => pinToNewRegister(nonReg)
  }

  def pin(source: DfVarOrValue): DfVarOrValue = source match {
    case reg: DfRegister => reg
    case value: DfConcreteAny => value
    case nonReg => pinToNewRegister(nonReg)
  }

  def newRegister(): DfRegister =
    new DfRegister(newRegisterId())

  def binaryOp(target: DfVariable, left: DfVarOrValue, op: String, right: DfVarOrValue): this.type = {
    newInstr(new BinaryOp(target, left, op, right))
    this
  }

  def noop(source: DfVarOrValue): this.type = {
    if (!source.isInstanceOf[DfRegister]) {
      newInstr(new Noop(source))
    }
    this
  }

  def read(target: DfVariable, base: DfVarOrValue, member: String): this.type = {
    newInstr(new ReadProp(target, base, member))
    this
  }

  def write(base: DfVarOrValue, member: String, value: DfVarOrValue): this.type = {
    newInstr(new WriteProp(base, member, value))
    this
  }

  def ret(source: DfVarOrValue): this.type = {
    newInstr(new Ret(source))
    this
  }

  def end(): this.type = {
    newInstr(new End)
    this
  }

  def instantiate(ret: DfVariable): this.type = {
    newInstr(new New(ret))
    this
  }

  def call(ret: Option[DfVariable], func: DfVarOrValue, params: Seq[DfVarOrValue]): this.type = {
    newInstr(new Call(ret, func, params))
    this
  }

  def unify(maybeRet: Option[DfVariable], elementEntities: Seq[DfVarOrValue]): this.type = {
    newInstr(new Unify(maybeRet, elementEntities))
    this
  }

  def jumpTo(target: BuildLabel): this.type = {
    newInstr(new Jump(target))
    this
  }

  def jumpIfFalse(condition: DfVarOrValue, target: BuildLabel): this.type = {
    newInstr(new JumpIfNot(condition, target))
    this
  }

  def debug(checks: Seq[Debug.Check]): this.type = {
    newInstr(new Debug(checks))
    this
  }

  def report(line: Int, before: Boolean): this.type = {
    newInstr(new Debug(Seq(Debug.Report(line, before))))
    this
  }

  def undefined: DfUndefined.type = DfValue.undefined
  def boolean(value: Boolean): DfConcreteBoolean = DfValue.boolean(value)
  def int(value: Int): DfConcreteInt = DfValue.int(value)
  def string(value: String): DfConcreteString = stringLiteralCache.getOrElseUpdate(value, DfConcreteString(value))

  def bindLabel(label: BuildLabel): this.type = {
    if (label.isBound)
      throw new IllegalArgumentException(s"Cannot bind bound label $label")

    if (!unboundLabels.contains(label))
      throw new IllegalArgumentException(s"Label $label belongs to another builder")

    unboundLabels -= label
    boundLabels += label
    numLabelsToNextInstr += 1
    label._targetIndex = indexOfNextInstr
    this
  }

  def tryBindLabel(label: Option[BuildLabel]): this.type = {
    label.foreach { label =>
      bindLabel(label)
    }
    this
  }

  def createLabel(name: String = ""): BuildLabel = {
    val label = new BuildLabel(name)
    unboundLabels += label
    label
  }

  def createSubBuilder(): CfgBuilder = new CfgBuilder()

  def build(): ControlFlowGraph = {
    val usedUnbound = usedLabels & unboundLabels.toSet[Label]
    if (usedUnbound.nonEmpty) {
      throw new IllegalStateException(s"Cannot build cfg with ${usedUnbound.size} unbound labels: ${usedUnbound.mkString(", ")}")
    }

    if (numLabelsToNextInstr > 0) {
      throw new IllegalStateException("Cannot build cfg with labels pointing after its end")
    }

    if (hasControlFlowFromPreviousInstruction) {
      throw new IllegalStateException("Cfg is not closed")
    }

    val graph = ControlFlowGraph(instructions.toArray)

    for (label <- boundLabels) {
      label._graph = graph
    }

    graph
  }
}

object CfgBuilder {
  class BuildLabel(_name: String) extends Label {
    private[CfgBuilder] var _targetIndex = -1
    private[CfgBuilder] var _graph: ControlFlowGraph = _

    override def name: String = makeName(insertLineNumber = true)
    override def nameWithoutLine: String = makeName(insertLineNumber = false)

    private def makeName(insertLineNumber: Boolean): String = {
      val boundTo = if (isBound) line.toString else "<unbound>"
      val name = if (_name.nonEmpty) _name else "unknown"

      if (!isBound || insertLineNumber) s"$name[$boundTo]"
      else name
    }

    override def targetIndex: Int = {
      assert(_targetIndex >= 0)
      _targetIndex
    }

    override def graph: ControlFlowGraph = {
      assert(_graph != null)
      _graph
    }

    def isBound: Boolean = {
      _targetIndex >= 0
    }
  }
}
