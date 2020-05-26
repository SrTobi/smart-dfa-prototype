package de.srtobi.dfaTest
package dfa

import de.srtobi.dfaTest.dfa.impl.{DataFlowAnalysisImpl, DfaPrinting}

object Main {
  def main(args: Array[String]): Unit = {
    val code =
      """
        |x = rand()
        |if (x == 3) {
        | a = "b"
        |} else {
        | a = "a"
        |}
        |//debug(x is debug.abs_any)
        |
        |/*if (rand()) {
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
        |}*/
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
