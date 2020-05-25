package de.srtobi.dfaTest
package dfa
package impl

import de.srtobi.dfaTest.cfg.Instruction
import de.srtobi.dfaTest.dfa.impl.constraints.{Constraint, EqualityConstraint}

import scala.collection.mutable
import scala.util.control.NonFatal

class DataFlowAnalysisImpl(stdLib: Seq[(String, DfConcreteAny)] = DataFlowAnalysisImpl.stdLib) extends DataFlowAnalysis {
  override type State = ExecutionState
  override type InstructionPtr = cfg.Instruction

  private val allocationSites = mutable.Map.empty[InstructionPtr, DfConcreteObjectRef]

  override def instructionAt(instructionPtr: cfg.Instruction): cfg.Instruction = instructionPtr

  override def instructionSorting: Ordering[Instruction] = Ordering.by(-_.index)

  override def preludePtr: cfg.Instruction = null
  override def initialState(instructions: cfg.ControlFlowGraph, input: Seq[(String, DfConcreteAny)]): (InstructionPtr, State) =
    instructions.entryInstruction -> ExecutionState.from(stdLib ++ input)

  override def process(instruction: InstructionPtr, states: Iterable[State]): Seq[(InstructionPtr, State)] = try {
    var instructionPinUsed = false
    def instructionPin(name: String): PinnedValue = {
      assert(!instructionPinUsed)
      instructionPinUsed = true
      PinnedValue(instruction)(name)
    }

    implicit class ExecutionStateExt(val executionState: ExecutionState) {
      def withView(constraint: Constraint, name: String): (PinnedValue, ExecutionState) = {
        val pin = instructionPin(name)
        pin -> executionState.withFact(_.withView(pin, constraint))
      }

      def withConditionalStore(target: DfVariable, value: DfValue, condition: Constraint, name: String): ExecutionState = {
        val pin = instructionPin(name)
        executionState.copy(
          variables = executionState.variables + (target -> DfPinned(pin)),
          facts = executionState.facts.withConditionalPin(pin, value, condition)
        )
      }
    }

    val state = states.reduce(_ unify _)

    reportInstructionSummary(instruction, state)

    def load(entity: DfVarOrValue): DfValue = state.resolve(entity)

    val nextState = instruction match {
      case cfg.Noop(_) =>
        state

      case _: cfg.End =>
        return Seq.empty

      case cfg.Mov(target, source) =>
        val s = load(source)
        target match {
          case _: DfRegister =>
            state.withStore(target, s)
          case _: DfLocalVariable =>
            state.facts.claimConstraint match {
              case Some(claimConstraint) =>
                state.withConditionalStore(target, s, claimConstraint, s"[$target = $source]")
              case None =>
                state.withStore(target, s)
            }
        }

      case cfg.New(target) =>
        val ref = allocationSites.getOrElseUpdate(instruction, new DfConcreteObjectRef)
        state.withStore(target, ref)

      case cfg.Jump(targetLabel) =>
        return Seq(targetLabel.target -> state)

      case cfg.BinaryOp(target, lhsSource, op, rhsSource) =>
        val lhs = load(lhsSource)
        val rhs = load(rhsSource)
        val (pin, result) = op match {
          case "==" => state.withView(EqualityConstraint(lhs, rhs), s"$lhsSource == $rhsSource")
          //case "+" => state.withPinnedStore(target, AdditionConstraint, lhs, rhs)
          case _ => ???
        }
        result.withStore(target, DfPinned(pin))

      case cfg.JumpIfNot(condition, targetLabel) =>
        def toNext(facts: Option[Facts], target: InstructionPtr): Seq[(InstructionPtr, State)] =
          facts.map(f => state.copy(facts = f)).map(target -> _).toSeq

        val cond = load(condition)
        val (ifTrue, ifFalse) = state.facts.claim(cond)
        return toNext(ifTrue, instruction.nextInstruction) ++
          toNext(ifFalse, targetLabel.target)

      case cfg.Debug(checks) =>
        import cfg.Debug._
        val line = instruction.lineNumber
        checks.foreach {
          case CheckDeadCode =>
            println("Instruction should be dead: " + instruction.asmLine)
          case CheckLiveCode =>
          case Is(actualSrc, expectation, exprText) =>
            import de.srtobi.dfaTest.cfg.Debug.Expectation
            val actual = load(actualSrc)

            expectation match {
              case Expectation.VarOrValue(varOrValue) =>
                val expected = load(varOrValue)
                if (actual.normalize(state) != expected) {
                  reportDebugError(s"Assertion '$exprText' in line $line failed ($actual != $expected)")
                }
              case Expectation.SubclassOf(expectedClass) =>
                val actualClass = actual.normalize(state).getClass
                if (!expectedClass.isAssignableFrom(actualClass)) {
                  reportDebugError(s"Assertion '$exprText' in line $line failed ($actualClass ⊂ $expectedClass)")
                }
              case Expectation.Value(expected) =>
                if (actual.normalize(state) != expected) {
                  reportDebugError(s"Assertion '$exprText' in line $line failed ($actual != $expected)")
                }
            }

          case Print(entity, exprText) =>
            val actual = load(entity).toString(state)
            reportDebugMessage(s"$exprText in $line: $actual")

          case Report(line, before) =>
            debugReport(instruction, state, line, before)
        }
        state

      case cfg.Ret(_) =>
        ???
      case cfg.Call(target, func, argSources) =>
        //val args = argSources.map(load)
        val result: DfValue =
          load(func) match {
            case DfConcreteInternalFunc("rand") =>
              DfPinned.fromAnchor(instruction, "rand in " + instruction.lineNumber)
            case _ => ???
          }
        target.fold(state)(state.withStore(_, result))

      case cfg.ReadProp(variable, value, str) =>
        ???
      case cfg.WriteProp(_, _, _) =>
        ???
      case cfg.Unify(target, elements) =>
        target.fold(state)(state.withStore(_, DfValue.unify(elements.map(load))))
    }
    Seq(instruction.nextInstruction -> nextState)
  } catch {
    case NonFatal(e) =>
      throw new Exception(s"Exception while processing '$instruction' in ${instruction.lineNumber}: '" + e.getMessage, e)
  }

  protected def reportInstructionSummary(instruction: InstructionPtr, state: State): Unit = ()
  protected def debugReport(instruction: InstructionPtr, state: State, line: Int, before: Boolean): Unit = ()
  protected def reportDebugMessage(msg: String): Unit = println(msg)
  protected def reportDebugError(msg: String): Unit = println(msg)
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
    val dfa = new DataFlowAnalysisImpl
    val runner = new DfaRunner(dfa)(cfg)
    runner.run()
  }
}