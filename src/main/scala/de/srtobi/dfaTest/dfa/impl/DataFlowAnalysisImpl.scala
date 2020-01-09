package de.srtobi.dfaTest
package dfa
package impl

import scala.collection.mutable

class DataFlowAnalysisImpl extends DataFlowAnalysis {
  override type State = ExecutionState
  override type InstructionPtr = cfg.Instruction

  private val allocationSites = mutable.Map.empty[InstructionPtr, DfConcreteObjectRef]

  override def instructionAt(instructionPtr: cfg.Instruction): cfg.Instruction = instructionPtr

  override def preludePtr: cfg.Instruction = null
  override def initialState(instructions: cfg.ControlFlowGraph): (InstructionPtr, State) =
    instructions.entryInstruction -> ExecutionState(Map.empty)

  override def process(instruction: InstructionPtr, states: Iterable[State]): Seq[(InstructionPtr, State)] = {
    val state = states.reduce(_ unify _)

    def load(entity: DfEntity): DfAbstractValue = entity match {
      case variable: DfVariable => state.variables.getOrElse(variable, DfUndefined)
      case value: DfAbstractValue => value
    }

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

      case cfg.JumpIfNot(condition, targetLabel) =>
        val result = load(condition)
        result match {
          case DfFalse | DfUndefined | DfConcreteInt(0) =>
            return Seq(targetLabel.target -> state)

          case _: DfConcreteAny =>
            state

          case value: DfAbstractAny =>
            ???
        }
    }
    Seq(instruction.nextInstruction -> nextState)
  }
}
