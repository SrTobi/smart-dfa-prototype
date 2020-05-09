package de.srtobi.dfaTest
package dfa

import de.srtobi.dfaTest.cfg.ControlFlowGraph

trait DataFlowAnalysis {
  type State
  type InstructionPtr

  def instructionAt(instructionPtr: InstructionPtr): cfg.Instruction

  def preludePtr: InstructionPtr
  def initialState(instructions: ControlFlowGraph, prelude: Seq[(String, DfConcreteAny)]): (InstructionPtr, State)

  def process(instruction: InstructionPtr, states: Iterable[State]): Seq[(InstructionPtr, State)]
}
