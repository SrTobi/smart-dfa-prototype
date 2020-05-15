package de.srtobi.dfaTest
package dfa
package impl

import de.srtobi.dfaTest.dfa.impl.constraints.{Constraint, EqualityConstraint}

import scala.collection.mutable
import scala.util.control.NonFatal

class DataFlowAnalysisImpl(stdLib: Seq[(String, DfConcreteAny)] = DataFlowAnalysisImpl.stdLib) extends DataFlowAnalysis {
  override type State = ExecutionState
  override type InstructionPtr = cfg.Instruction

  private val allocationSites = mutable.Map.empty[InstructionPtr, DfConcreteObjectRef]

  override def instructionAt(instructionPtr: cfg.Instruction): cfg.Instruction = instructionPtr

  override def preludePtr: cfg.Instruction = null
  override def initialState(instructions: cfg.ControlFlowGraph, input: Seq[(String, DfConcreteAny)]): (InstructionPtr, State) =
    instructions.entryInstruction -> ExecutionState.from(stdLib ++ input)

  override def process(instruction: InstructionPtr, states: Iterable[State]): Seq[(InstructionPtr, State)] = try {
    implicit class ExecutionStateExt(val executionState: ExecutionState) {
      def withConstraint(constraintF: PinnedValue => Constraint, name: String): (PinnedValue, ExecutionState) = {
        val pin = PinnedValue(instruction)(name)
        pin -> executionState.withConstraint(constraintF(pin))
      }
    }

    val state = states.reduce(_ unify _)

    def load(entity: DfVarOrValue): DfValue = state.resolve(entity)

    val nextState = instruction match {
      case cfg.Noop(_) =>
        state

      case _: cfg.End =>
        return Seq.empty

      case cfg.Mov(target, source) =>
        state.withStore(target, load(source))

      case cfg.New(target) =>
        val ref = allocationSites.getOrElseUpdate(instruction, new DfConcreteObjectRef)
        state.withStore(target, ref)

      case cfg.Jump(targetLabel) =>
        return Seq(targetLabel.target -> state)

      case cfg.BinaryOp(target, lhsSource, op, rhsSource) =>
        val lhs = load(lhsSource)
        val rhs = load(rhsSource)
        val (pin, result) = op match {
          case "==" => state.withConstraint(EqualityConstraint(_, lhs, rhs), s"$lhs == $rhs")
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
                  println(s"Assertion '$exprText' in line $line failed ($actual != $expected)")
                }
              case Expectation.SubclassOf(expectedClass) =>
                val actualClass = actual.normalize(state).getClass
                if (!expectedClass.isAssignableFrom(actualClass)) {
                  println(s"Assertion '$exprText' in line $line failed ($actualClass ⊂ $expectedClass)")
                }
              case Expectation.Value(expected) =>
                if (actual.normalize(state) != expected) {
                  println(s"Assertion '$exprText' in line $line failed ($actual != $expected)")
                }
            }

          case Print(entity, exprText) =>
            val actual = load(entity).toString(state)
            println(s"$exprText in $line: $actual")
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
    }
    Seq(instruction.nextInstruction -> nextState)
  } catch {
    case NonFatal(e) =>
      throw new Exception(s"Exception while processing '$instruction' in ${instruction.lineNumber}: '" + e.getMessage, e)
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
        |a = 1
        |b = rand()
        |debug.print(a)
        |if (a == b) {
        |  if (a == b) {
        |  }
        |  debug.print(a)
        |}
        |debug.print(a)
        |debug(a is debug.any)
        |""".stripMargin
    val cfg = CfgTransformer.transformScript(LangParser.parse(code))
    println(cfg.asmText())
    println("---------------------------")
    val dfa = new DataFlowAnalysisImpl
    val runner = new DfaRunner(dfa)(cfg)
    runner.run()
  }
}