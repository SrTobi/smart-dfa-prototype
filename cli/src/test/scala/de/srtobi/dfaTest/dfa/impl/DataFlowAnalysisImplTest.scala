package de.srtobi.dfaTest.dfa.impl

import de.srtobi.dfaTest.dfa.DfaRunner
import de.srtobi.dfaTest.{CfgTransformer, LangParser}
import org.scalatest.funsuite.AnyFunSuite

class DataFlowAnalysisImplTest extends AnyFunSuite {
  test("easy") {
    run(
      """
        |x = rand()
        |if (x == 3) {
        | a = "b"
        |} else {
        | a = "a"
        |}
        |debug(x is debug.abs_any)
        |
        |if (rand()) {
        | b = "b"
        |} else {
        | b = "c"
        |}
        |
        |debug(x is debug.any)
        |debug(a is ["a" | "b"])
        |debug(b is ["b" | "c"])
        |
        |if (a == b) {
        |  debug(x is 3)
        |  debug(a is "b")
        |  debug(b is "b")
        |}
        |""".stripMargin
    )
  }


  def run(code: String): Unit = {
    val cfg = CfgTransformer.transformScript(LangParser.parse(code))
    //println(cfg.asmText())
    //println("---------------------------")
    val errorBuilder = Seq.newBuilder[String]
    val dfa = new DataFlowAnalysisImpl {
      override protected def reportDebugError(msg: String): Unit =
        errorBuilder += msg
    }
    val runner = new DfaRunner(dfa)(cfg)
    runner.run()

    val errors = errorBuilder.result()
    if (errors.nonEmpty) {
      fail("Errors found!" + errors.mkString("\n", "\n", "\n"))
    }
  }
}
