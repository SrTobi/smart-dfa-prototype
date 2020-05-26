package de.github.srtobi.dfaTest.webapi

import de.srtobi.dfaTest.dfa.DfaRunner
import de.srtobi.dfaTest.dfa.impl.{DataFlowAnalysisImpl, DfaAsmPrinting, DfaPrinting}
import de.srtobi.dfaTest.{CfgTransformer, LangParser}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("SimpleDfaApi")
object SimpleApi {
  @JSExport
  def analyse(code: String, asmOutput: Boolean): String = {

    val ast = LangParser.parse(code)
    val cfg = CfgTransformer.transformScriptWithReports(ast, code)
    //println(cfg.asmText())
    //println("---------------------------")

    if (asmOutput) {
      val dfa = new DataFlowAnalysisImpl with DfaAsmPrinting
      val runner = new DfaRunner(dfa)(cfg)
      runner.run()
      dfa.printIn(cfg)
    } else {
      val dfa = new DataFlowAnalysisImpl with DfaPrinting
      val runner = new DfaRunner(dfa)(cfg)
      runner.run()
      dfa.printIn(code)
    }
  }
}
