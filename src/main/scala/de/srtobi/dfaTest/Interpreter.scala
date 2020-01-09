package de.srtobi.dfaTest

import de.srtobi.dfaTest.cfg.ControlFlowGraph

import scala.collection.mutable

class Interpreter(scriptCfg: ControlFlowGraph, stdLib: Seq[(String, DfConcreteValue)] = Interpreter.stdLib)(input: (String, DfConcreteValue)*) {
  private var objects = mutable.Map.empty[DfConcreteObjectRef, mutable.Map[String, DfConcreteValue]]
  private var curStackFrame = Option(new StackFrame(scriptCfg, mutable.Map.from(input), None, None))

  def instantiateObject(props: (String, DfConcreteValue)*): DfConcreteObjectRef = {
    val ref = new DfConcreteObjectRef
    objects += ref -> mutable.Map.empty
    for ((prop, value) <- props) {
      writeObj(ref, prop)(value)
    }
    ref
  }

  def writeObj(obj: DfConcreteObjectRef, prop: String)(value: DfConcreteValue): Unit = {
    objects(obj) += prop -> value
  }

  def readObj(obj: DfConcreteObjectRef, prop: String): DfConcreteValue = {
    objects(obj)(prop)
  }

  def step(): Unit = {
    curStackFrame.get.process()
  }

  def run(): Unit = {
    while (curStackFrame.isDefined) {
      step()
    }
  }

  private class StackFrame(funcCfg: ControlFlowGraph, vars: mutable.Map[String, DfConcreteValue],
                           parent: Option[StackFrame], returnTarget: Option[DfVariable]) {
    private var registered = mutable.Map.empty[DfRegister, DfConcreteValue]
    private var curInstrIndex = 0

    locally {
      vars ++= stdLib
    }

    def process(): Unit = {
      val cur = funcCfg.instructionAt(curInstrIndex)
      executeInstruction(cur)
      if (curInstrIndex == cur.index) {
        curInstrIndex += 1
      }
    }

    private def load(entity: DfEntity): DfConcreteValue = entity match {
      case reg: DfRegister => registered(reg)
      case DfLocalVariable(name) => vars.getOrElseUpdate(name, DfUndefined)
      case value: DfConcreteValue => value
      case _: DfAbstractValue => throw new Exception("Cannot load abstract value")
    }

    private def store(target: DfVariable, value: DfConcreteValue): Unit = target match {
      case reg: DfRegister => registered += reg -> value
      case variable: DfVariable => vars += variable.name -> value
    }

    @scala.annotation.tailrec
    private def asObj(value: DfEntity): DfConcreteObjectRef = value match {
      case v: DfVariable => asObj(load(v))
      case obj: DfConcreteObjectRef => obj
      case _ => throw new Exception(s"$value is not an object")
    }

    private def executeInstruction(instruction: cfg.Instruction): Unit = instruction match {
      case cfg.Mov(target, source) =>
        store(target, load(source))

      case cfg.WriteProp(base, member, source) =>
        writeObj(asObj(base), member)(load(source))

      case cfg.ReadProp(target, base, member) =>
        store(target, readObj(asObj(base), member))

      case cfg.BinaryOp(target, leftNode, op, rightNode) =>
        val left = load(leftNode)
        val right = load(rightNode)
        def int(df: DfConcreteValue): Int = df match {
          case DfConcreteInt(i) => i
          case _ => throw new Exception(s"Cannot convert $df into int")
        }
        val result = op match {
          case "==" => DfValue.boolean(left == right)
          case "!=" => DfValue.boolean(left != right)
          case "+" => DfValue.int(int(left) + int(right))
          case "-" => DfValue.int(int(left) - int(right))
        }
        store(target, result)

      case cfg.Call(maybeTarget, func, args) =>
        val pinnedArgs = args.map(load)
        load(func) match {
          case lambda: DfConcreteLambdaRef =>
            val vars = lambda.params.map(_.name).zip(pinnedArgs)
            val newFrame = new StackFrame(lambda.cfg, mutable.Map.from(vars), Some(StackFrame.this), maybeTarget)
            curStackFrame = Some(newFrame)

          case DfConcreteInternalFunc(func) =>
            val ret = func match {
              case "rand" =>
                DfValue.boolean(math.random() < 0.5)
              case "print" =>
                println(pinnedArgs.mkString(" "))
                DfValue.undefined
            }
            maybeTarget.foreach(store(_, ret))

          case f =>
            throw new Exception(s"Cannot call $f")
        }

      case cfg.New(target) =>
        store(target, instantiateObject())

      case cfg.Jump(targetLabel) =>
        curInstrIndex = targetLabel.targetIndex

      case cfg.JumpIfNot(condition, targetLabel) =>
        val cond = load(condition)
        cond match {
          case DfConcreteInt(0) | DfUndefined | DfFalse =>
            curInstrIndex = targetLabel.targetIndex

          case _ =>
        }

      case cfg.Ret(returnValue) =>
        parent.fold(throw new Exception("Cannot return from top level")) { parent =>
          returnTarget.foreach(parent.store(_, load(returnValue)))
          curStackFrame = Some(parent)
        }

      case cfg.End() =>
        if (parent.isDefined) throw new Exception("Cannot end in lambda")
        curStackFrame = None

      case cfg.Noop(_) => ()
      case cfg.Debug(checks) =>
        import cfg.Debug._
        checks.foreach {
          case CheckDeadCode =>
            println(s"Instruction ${instruction.index} should be dead code: ${instruction.asmString}")
          case CheckLiveCode =>
          case Print(entity, exprText) =>
            println(exprText + ": " + load(entity))
          case Is(actualEntity, expectedEntity, exprText) =>
            val actual = load(actualEntity)
            val expected = load(expectedEntity)
            if (actual != expected) {
              println(s"Assertion '$exprText' failed ($actual != $expected)")
            }
        }
    }
  }
}

object Interpreter {
  val stdLib = Seq(
    "rand" -> DfConcreteInternalFunc("rand"),
    "print" -> DfConcreteInternalFunc("print")
  )

  def main(args: Array[String]): Unit = {
    val code =
      """
        |o = { rand: rand }
        |debug(4 + 5 is 9)
        |""".stripMargin
    val cfg = CfgTransformer.transformScript(LangParser.parse(code))
    println(cfg.asmText())
    println("---------------------------")
    val interpreter = new Interpreter(cfg)("input" -> DfValue.int(3))
    interpreter.run()
  }
}