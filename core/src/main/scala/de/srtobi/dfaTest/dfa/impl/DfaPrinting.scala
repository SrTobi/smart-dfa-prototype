package de.srtobi.dfaTest.dfa.impl

import scala.collection.mutable

trait DfaPrinting extends DataFlowAnalysisImpl {
  private val infos = mutable.Map.empty[(Int, Boolean), Map[DfVariable, DfAbstractAny]]

  override protected def debugReport(instruction: Instruction, state: ExecutionState, line: Int, before: Boolean): Unit = {
    val pinValues = state.facts.computedValues(state.variables.values.toSeq)
    val normalized = state.variables.view
      .mapValues {
        value =>
          val (v, pins) = value.deconstruct
          dfa.DfValue.unify(Iterator(v) ++ pins.map(pinValues.getOrElse(_, DfAny)))
      }.toMap

    infos += ((line -> before) -> normalized)
  }

   def printIn(code: String): String = {
     val builder = new StringBuilder
     val current = mutable.Map.empty[DfVariable, DfAbstractAny]

     def printForLine(indent: String, line: Int, before: Boolean): Unit = {
       for {
         info <- infos.get(line -> before)
         (v, value) <- info.iterator
           .filter(_._1.isInstanceOf[DfLocalVariable])
           .filter { case (v, value) => !current.get(v).contains(value) }
       } {
         current += (v -> value)
         builder ++= indent
         builder ++= s"// ${v.name} = $value\n"
       }
     }

     for ((line, num) <- code.linesIterator.zipWithIndex) {
       val indent = line.takeWhile(_.isSpaceChar)
       printForLine(indent, num, before = true)
       builder ++= line
       builder += '\n'
       printForLine(indent, num, before = false)
     }

     builder.result()
   }

  override protected def reportDebugMessage(msg: String): Unit = ()
  override protected def reportDebugError(msg: String): Unit = ()
}

object DfaPrintingMain {
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
    val cfg = CfgTransformer.transformScriptWithReports(LangParser.parse(code), code)
    //println(cfg.asmText())
    //println("---------------------------")
    val dfa = new DataFlowAnalysisImpl with DfaPrinting
    val runner = new DfaRunner(dfa)(cfg)
    runner.run()
    println(dfa.printIn(code))
  }
}