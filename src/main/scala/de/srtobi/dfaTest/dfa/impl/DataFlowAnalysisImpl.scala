package de.srtobi.dfaTest
package dfa
package impl

import de.srtobi.dfaTest.cfg.{Jump, JumpIfNot, New}

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

    def load(entity: DfEntity): DfValue = entity match {
      case variable: DfVariable => state.variables.getOrElse(variable, DfUndefined)
      case value: DfValue => value
    }

    val nextState = instruction match {
      case cfg.Noop(_) =>
        state

      case _: cfg.End =>
        return Seq.empty

      case cfg.Mov(target, source) =>
        state.withStore(target, load(source))

      case New(target) =>
        val ref = allocationSites.getOrElseUpdate(instruction, new DfConcreteObjectRef)
        state.withStore(target, ref)

      case Jump(targetLabel) =>
        return Seq(targetLabel.target -> state)

      case JumpIfNot(condition, targetLabel) =>
        val result = load(condition)
        result match {
          case value: DfAbstractValue =>
            ???
          case DfFalse | DfUndefined | DfConcreteInt(0) =>
            return Seq(targetLabel.target -> state)
          case _: DfConcreteValue =>
            state
        }
    }

    Seq(instruction.nextInstruction -> nextState)
  }
}
