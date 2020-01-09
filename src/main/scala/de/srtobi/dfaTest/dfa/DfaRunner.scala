package de.srtobi.dfaTest
package dfa

import de.srtobi.dfaTest.cfg.ControlFlowGraph
import de.srtobi.dfaTest.dfa.DfaRunner.WQItem

import scala.collection.mutable

class DfaRunner[DFA <: DataFlowAnalysis](val dfa: DFA)(val controlFlowGraph: ControlFlowGraph) {
  type State = dfa.State
  private type InstructionPtr = dfa.InstructionPtr
  private type Item = WQItem[State, InstructionPtr]

  private val enqueuedItems = mutable.Map.empty[InstructionPtr, Item]
  private val workQueue = mutable.Queue.empty[Item]

  locally {
    addToQueue(dfa.preludePtr, dfa.initialState(controlFlowGraph))
  }

  private def addToQueue(from: InstructionPtr, instructionAndState: (InstructionPtr, State)): Unit = {
    val (instruction, state) = instructionAndState
    val item = enqueuedItems.getOrElseUpdate(instruction, new Item(instruction))
    item.states += from -> state
    if (!item.enqueued) {
      workQueue.enqueue(item)
      item.enqueued = true
    }
  }

  def hasFinished: Boolean = workQueue.isEmpty

  def step(): Unit = {
    val item = workQueue.dequeue()
    item.enqueued = false
    val instructionPtr = item.instruction

    // free work queue item if there is only one input
    val instruction = dfa.instructionAt(instructionPtr)
    if (instruction.labels.isEmpty) {
      enqueuedItems -= instructionPtr
    }

    val nexts = dfa.process(instructionPtr, item.states.values)
    nexts.foreach(addToQueue(instructionPtr, _))
  }

  def run(): Unit =
    while (!hasFinished) step()
}

object DfaRunner {
  private class WQItem[State, InstructionPtr](val instruction: InstructionPtr) {
    var states: Map[InstructionPtr, State] = Map.empty
    var enqueued: Boolean = false
  }
}