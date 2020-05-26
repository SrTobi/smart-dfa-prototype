package de.srtobi.dfaTest
package dfa
package impl

import scala.collection.mutable

trait DfaAsmPrinting extends DataFlowAnalysisImpl {
  private val infos = mutable.Map.empty[InstructionPtr, Map[DfVariable, DfAbstractAny]]

  override protected def reportInstructionSummary(instruction: Instruction, state: ExecutionState): Unit = {
    val pinValues = state.facts.computedValues(state.variables.values.toSeq)
    val normalized = state.variables.view
      .mapValues {
        value =>
          val (v, pins) = value.deconstruct
          dfa.DfValue.unify(Iterator(v) ++ pins.map(pinValues.getOrElse(_, DfAny)))
      }.toMap

    infos += (instruction -> normalized)
  }

  def printIn(graph: cfg.ControlFlowGraph): String = {
    if (graph.instructions.isEmpty) {
      return "<empty-cfg>"
    }

    val current = mutable.Map.empty[DfVariable, DfAbstractAny]
    val builder = new StringBuilder

    for ((instr, idx) <- graph.instructions.zipWithIndex) {
      if (idx > 0)
        builder.append("\n")

      if (instr.labels.nonEmpty) {
        for ((label, idx) <- instr.labels.zipWithIndex) {
          if (idx > 0)
            builder.append(" ")
          builder.append(label)
        }
        builder.append(":\n")
      }

      for {
        info <- infos.get(instr)
        (v, value) <- info.iterator
          .filter { case (v, value) => v.isInstanceOf[DfLocalVariable] && !current.get(v).contains(value) }
      } {
        current += (v -> value)
        builder ++= s"        // ${v.name} = $value\n"
      }

      builder.append(instr.lineNumber)
      builder.append(": ")

      builder.append(instr.asmString)
    }

    for (lambda <- graph.lambdas) {
      builder.append("\n\n# ")
      builder.append(lambda)
      builder.append("\n")
      builder.append(printIn(lambda.cfg))
    }

    builder.result()
  }

  override protected def reportDebugMessage(msg: String): Unit = ()
  override protected def reportDebugError(msg: String): Unit = ()
}


object DfaAsmPrintingMain {
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
    //println(cfg.asmText())
    //println("---------------------------")
    val dfa = new DataFlowAnalysisImpl with DfaAsmPrinting
    val runner = new DfaRunner(dfa)(cfg)
    runner.run()
    println(dfa.printIn(cfg))
  }
}