package de.srtobi.dfaTest
package dfa
package impl2

import de.srtobi.dfaTest.cfg.Instruction

import scala.collection.mutable

class ConstraintAnalysis(stdLib: Seq[(String, DfConcreteAny)] = DataFlowAnalysisImpl.stdLib) extends DataFlowAnalysis {
  override type State = GatherState
  override type InstructionPtr = cfg.Instruction

  override def instructionAt(instructionPtr: cfg.Instruction): cfg.Instruction = instructionPtr

  override def instructionSorting: Ordering[Instruction] = Ordering.by(-_.index)

  override def preludePtr: cfg.Instruction = null
  override def initialState(instructions: cfg.ControlFlowGraph, input: Seq[(String, DfConcreteAny)]): (InstructionPtr, State) =
    instructions.entryInstruction -> GatherState.empty

  var result: Option[GatherState] = None

  override def process(instruction: InstructionPtr, states: Iterable[State]): Seq[(InstructionPtr, State)] = try {
    val state = GatherState.unify(states)

    def load(value: DfVarOrValue): Value = {
      value match {
        case reg: DfRegister => state.register(reg)
        case v: DfVariable => state.readProp(state.global.localScope, v.name)
        case v: DfAbstractAny => state.constant(v)
      }
    }

    def store(target: DfVariable, value: Value): Unit = {
      target match {
        case reg: DfRegister => state.newRegister(reg, value)
        case v: DfVariable => state.writeProp(state.global.localScope, v.name, value)
        case v: DfAbstractAny => state.constant(v)
      }
    }

    instruction match {
      case cfg.Noop(_) =>
      case _: cfg.End =>
        result = Some(state)
        return Seq.empty

      case cfg.Mov(target, source) =>
        store(target, load(source))

      case cfg.New(target) =>
        store(target, state.constant(new DfConcreteObjectRef(s"obj in ${instruction.lineNumber}")))

      case cfg.Jump(targetLabel) =>
        return Seq(targetLabel.target -> state)

      case cfg.BinaryOp(target, lhsSource, op, rhsSource) =>
        val lhs = load(lhsSource)
        val rhs = load(rhsSource)
        val result = op match {
          case "==" => state.makeEquality(lhs, rhs)
          case _ => ???
        }
        store(target, result)

      case cfg.JumpIfNot(condition, targetLabel) =>
        val cond = load(condition)
        val (ifTrue, ifFalse) = state.split(cond)
        return Seq(
          instruction.nextInstruction -> ifTrue,
          targetLabel.target -> ifFalse,
        )

      case cfg.Debug(checks) =>
        import cfg.Debug._

        val line = instruction.lineNumber
        checks.foreach {
          case CheckDeadCode =>
            //println("Instruction should be dead: " + instruction.asmLine)
            ???
          case CheckLiveCode =>
            ???
          case Is(actualSrc, expectation, exprText) =>

//            val actual = load(actualSrc)
//
//            expectation match {
//              case Expectation.VarOrValue(varOrValue) =>
//                val expected = load(varOrValue)
//                if (actual.normalize(state) != expected) {
//                  reportDebugError(s"Assertion '$exprText' in line $line failed ($actual != $expected)")
//                }
//                ???
//              case Expectation.SubclassOf(expectedClass) =>
//                val actualClass = actual.normalize(state).getClass
//                if (!expectedClass.isAssignableFrom(actualClass)) {
//                  reportDebugError(s"Assertion '$exprText' in line $line failed ($actualClass ⊂ $expectedClass)")
//                }
//              case Expectation.Value(expected) =>
//                if (actual.normalize(state) != expected) {
//                  reportDebugError(s"Assertion '$exprText' in line $line failed ($actual != $expected)")
//                }
//            }
            ???

          case Print(entity, exprText) =>
            //val actual = load(entity).toString(state)
            //reportDebugMessage(s"$exprText in $line: $actual")
            ???
          case report: Report =>
            state.report(report)
        }

      case cfg.Ret(_) =>
        ???
      case cfg.Call(target, func, argSources) =>
        //val args = argSources.map(load)
        /*val result: DfValue =
          load(func) match {
            case DfConcreteInternalFunc("rand") =>
              DfPinned.fromAnchor(instruction, "rand in " + instruction.lineNumber)
            case _ => ???
          }
        target.fold(state)(state.withStore(_, result))*/
        load(func) match {
          case c: Constant =>
            c.value match {
              case DfConcreteInternalFunc("rand") =>
                target.foreach(
                  store(_, state.unknownValue("result of rand"))
                )

              case _ => ???
            }
          case _ => ???
        }

      case cfg.ReadProp(target, base, prop) =>
        store(target, state.readProp(load(base), prop))
      case cfg.WriteProp(base, prop, value) =>
        state.writeProp(load(base), prop, load(value))

      case cfg.Unify(target, elements) =>
        target.foreach(store(_, state.unify(elements.iterator.map(load).toSet)))
    }
    Seq(instruction.nextInstruction -> state)
  }
}

object DataFlowAnalysisImpl {
  val stdLib: Seq[(String, DfConcreteAny)] = Seq(
    "rand" -> DfConcreteInternalFunc("rand")
  )
}



object DfaTest {
  def main(args: Array[String]): Unit = {
    val code =
      """
        |x = rand()
        |if (x == 3) {
        | a = "b"
        |} else {
        | a = "a"
        |}
        |
        |if (rand()) {
        | b = "b"
        |} else {
        | b = "c"
        |}
        |
        |debug(a is ["a" | "c"])
        |debug.print(x)
        |
        |if (a == b) {
        |  debug(a is "b")
        |  debug(x is 3)
        |}
        |
        |""".stripMargin
    val cfg = CfgTransformer.transformScript(LangParser.parse(code))
    println(cfg.asmText())
    println("---------------------------")
    val dfa = new ConstraintAnalysis()
    val runner = new DfaRunner(dfa)(cfg)
    runner.run()
  }
}
