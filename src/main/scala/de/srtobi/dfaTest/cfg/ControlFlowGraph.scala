package de.srtobi.dfaTest.cfg

import de.srtobi.dfaTest.cfg
import de.srtobi.dfaTest.dfa.DfConcreteLambdaRef

class ControlFlowGraph private (val instructions: Array[cfg.Instruction], val lambdas: Array[DfConcreteLambdaRef]) {
  assert(instructions.length > 0)

  def instructionCount: Int = instructions.length
  def instructionAt(index: Int): cfg.Instruction = instructions(index)

  def entryInstruction: cfg.Instruction = instructions.head

  def asmText(lineNumbers: Boolean = true, labels: Boolean = true, indentation: Boolean = true): String = {
    if (instructions.isEmpty) {
      return "<empty-cfg>"
    }

    val builder = new StringBuilder

    for ((instr, idx) <- instructions.zipWithIndex) {
      if (idx > 0)
        builder.append("\n")

      if (labels && instr.labels.nonEmpty) {
        for ((label, idx) <- instr.labels.zipWithIndex) {
          if (idx > 0)
            builder.append(" ")
          builder.append(label)
        }
        builder.append(":\n")
      }

      if (lineNumbers) {
        val line = idx + 1
        builder.append(line)
        builder.append(": ")
      }

      if (indentation && !lineNumbers) {
        builder.append("  ")
      }

      builder.append(instr.asmString)
    }

    for (lambda <- lambdas) {
      builder.append("\n\n# ")
      builder.append(lambda)
      builder.append("\n")
      builder.append(lambda.cfg.asmText(lineNumbers, labels, indentation))
    }

    builder.toString()
  }

  override def toString: String = asmText()
}

object ControlFlowGraph {
  private[cfg] def apply(instructions: Array[cfg.Instruction]): ControlFlowGraph = {
    val lambdas =
      instructions.flatMap(_.sourceEntities.collect { case lambda: DfConcreteLambdaRef => lambda })

    val graph = new ControlFlowGraph(instructions, lambdas)

    val jumps = instructions
      .collect { case ji: cfg.JumpingInstruction => ji }

    val labelsOfInstr = jumps
      .map(_.targetLabel)
      .groupBy(_.targetIndex)
      .view
      .mapValues(_.toSet)
      .toMap
      .withDefaultValue(Set.empty)


    for ((instr, idx) <- instructions.zipWithIndex) {
      cfg.Instruction.finalizeInstruction(instr, graph, labelsOfInstr(idx))
    }

    graph
  }
}