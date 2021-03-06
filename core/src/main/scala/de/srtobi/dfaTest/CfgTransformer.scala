package de.srtobi.dfaTest

import de.srtobi.dfaTest.cfg._
import de.srtobi.dfaTest.dfa.{DfConcreteLambdaRef, DfVarOrValue}

import scala.util.DynamicVariable

object CfgTransformer {
  private val insertReports = new DynamicVariable[Option[String]](None)

  def transformScript(script: Ast.Script): ControlFlowGraph = {
    this.insertReports.value = None
    implicit val builder: CfgBuilder = new CfgBuilder
    buildStatements(script.main)
    builder.end()
    builder.build()
  }

  def transformScriptWithReports(script: Ast.Script, code: String): ControlFlowGraph = {
    this.insertReports.value = Some(code)
    implicit val builder: CfgBuilder = new CfgBuilder
    buildStatements(script.main)
    builder.end()
    builder.build()
  }

  object DebugExpr {
    private var _isInDebugExpr: Boolean = false
    val debugIdentifier: Ast.Identifier = Ast.Identifier("debug")

    private def inDebugExpr[R](body: => R): R = {
      if (_isInDebugExpr) {
        throw new Exception("Cannot have debug expression in a debug expression")
      }
      _isInDebugExpr = true
      val result = body
      _isInDebugExpr = false
      result
    }

    def isInDebugExpr: Boolean = _isInDebugExpr

    def unapply(expr: Ast.Expression): Option[Seq[CfgBuilder => Debug.Check]] = {
      val fs: Seq[CfgBuilder => Debug.Check] = expr match {
        case Ast.Call(Ast.PropertyAccess(`debugIdentifier`, "print"), args) =>
          args.map(expr => (cfgBuilder: CfgBuilder) => {
            val result = transformExpr(expr, RequireResult)(cfgBuilder).get
            Debug.Print(result, LangPrinter.print(expr))
          })

        case Ast.Call(Ast.PropertyAccess(`debugIdentifier`, "liveCode"), args) =>
          if (args.nonEmpty)
            throw new Exception("debug.liveCode() cannot take arguments.")

          Seq(_ => Debug.CheckLiveCode)

        case Ast.Call(Ast.PropertyAccess(`debugIdentifier`, "deadCode"), args) =>
          if (args.nonEmpty)
            throw new Exception("debug.deadCode() cannot take arguments.")

          Seq(_ => Debug.CheckDeadCode)

        case Ast.Call(`debugIdentifier`, args) =>
          args.map(expr => (cfgBuilder: CfgBuilder) => expr match {
            case Ast.Operator(LangParser.debugIsOperator, lhsExpr, rhsExpr) =>
              val lhs = transformExpr(lhsExpr, RequireResult)(cfgBuilder).get
              val rhs = rhsExpr match {
                case DebugProperty(exp) => exp
                case expr =>
                  Debug.Expectation.VarOrValue(
                    transformExpr(expr, RequireResult)(cfgBuilder).get
                  )
              }

              Debug.Is(lhs, rhs, LangPrinter.print(expr))
            case _ =>
              throw new Exception("Expected is expression at root of debug call")
          })
        case _ =>
          null
      }

      Option(fs).map(_.map(f => cb => inDebugExpr(f(cb))))
    }
  }

  object DebugProperty {
    def unapply(expr: Ast.PropertyAccess): Option[Debug.Expectation] = expr match {
      case Ast.PropertyAccess(Ast.Identifier("debug"), name) if DebugExpr.isInDebugExpr =>
        Debug.Expectation.fromPropertyName(name)
      case _ =>
        None
    }
  }

  def transformExpr(expr: Ast.Expression, rreq: ResultRequirement)(implicit builder: CfgBuilder): ExprResult = expr match {
    case Ast.UndefinedLiteral =>
      rreq.satisfyUndefined(noop = true)

    case Ast.BooleanLit(value) =>
      rreq.satisfy(builder.boolean(value))

    case Ast.NumberLit(value) =>
      rreq.satisfy(builder.int(value))

    case Ast.StringLiteral(value) =>
      rreq.satisfy(builder.string(value))

    case Ast.Identifier(name) =>
      rreq.satisfy(builder.resolveVariable(name))

    case Ast.Union(elements) =>
      val elementEntities = elements.map(transformExpr(_, RequireResult).get)
      val (maybeRet, result) = rreq.tryPin()
      builder.unify(maybeRet, elementEntities)
      result

    case Ast.Object(props) =>
      val obj = builder.newRegister()
      builder.instantiate(obj)
      for (Ast.Property(name, init) <- props) {
        val result = transformExpr(init, RequireResult).get
        builder.write(obj, name, result)
      }
      rreq.satisfy(obj)

    case Ast.PropertyAccess(baseExpr, prop) =>
      val base = transformExpr(baseExpr, RequireResult).get
      val (maybePin, result) = rreq.tryPin()
      maybePin.foreach(builder.read(_, base, prop))
      result

    case Ast.Call(funcExpr, args) =>
      val func = transformExpr(funcExpr, RequireResult)
      val argEntities = args.map(transformExpr(_, RequireResult).get)
      val (maybeRet, result) = rreq.tryPin()
      builder.call(maybeRet, func, argEntities)
      result

    case f@Ast.Function(paramNames, block) =>
      val subBuilder = builder.createSubBuilder()
      val params = paramNames.map(builder.resolveVariable).map(new DfConcreteLambdaRef.Parameter(_))
      buildStatements(block)(subBuilder)
      subBuilder.end()
      val cfg = subBuilder.build()
      val lambda = new DfConcreteLambdaRef(f, params, cfg)
      rreq.satisfy(lambda)

    case Ast.Operator(op, leftExpr, rightExpr) =>
      if (op == LangParser.debugIsOperator) {
        throw new Exception("Cannot use 'is' operator in expression outside of debug(...)")
      }
      val left = transformExpr(leftExpr, RequireResult)
      val right = transformExpr(rightExpr, RequireResult)
      val (maybeRet, result) = rreq.tryPin()
      val ret = maybeRet.getOrElse(builder.newRegister())
      builder.binaryOp(ret, left, op, right)
      result
  }

  def transformStatement(statement: Ast.Statement)(implicit builder: CfgBuilder): Unit = builder.withIndex(statement.startIndex) {
    statement match {
      case Ast.ExpressionStmt(DebugExpr(checkFs)) =>
        val checks = checkFs.map(f => f(builder))
        builder.debug(checks)

      case Ast.ExpressionStmt(expr) =>
        transformExpr(expr, RequireNoResult)

      case Ast.IfStmt(condExpr, thenExpr, elseExpr) =>
        val hasElse = elseExpr.isDefined
        val endLabel = builder.createLabel("endIf")
        val elseLabel = if (hasElse) builder.createLabel("else") else endLabel

        val cond = transformExpr(condExpr, RequireResult)
        builder.jumpIfFalse(cond, elseLabel)
        buildStatements(thenExpr)
        if (hasElse) {
          builder.jumpTo(endLabel)

          builder.bindLabel(elseLabel)
          buildStatements(elseExpr.get)
        }
        builder.bindLabel(endLabel)

      case Ast.ReturnStmt(expr) =>
        val result = buildExprOrUndefined(expr)
        builder.ret(result.get)

      case Ast.AssignmentStmt(Ast.PropertyAccess(baseExpr, property), expr) =>
        val base = transformExpr(baseExpr, RequireResult).get
        val entity = transformExpr(expr, RequireResult).get

        builder.write(base, property, entity)

      case Ast.AssignmentStmt(Ast.Identifier(variable), expr) =>
        transformExpr(expr, RequireResult(builder.resolveVariable(variable)))

      case Ast.AssignmentStmt(_, _) =>
        throw new Exception("Can only assign to variables and properties")
    }
  }


  private def buildExprOr(expr: Option[Ast.Expression],
                          result: => DfVarOrValue,
                          rreq: ResultRequirement = RequireResult)(implicit builder: CfgBuilder): ExprResult =
    expr.map(transformExpr(_, rreq)).getOrElse(rreq.satisfy(result))

  private def buildExprOrUndefined(expr: Option[Ast.Expression],
                     rreq: ResultRequirement = RequireResult)(implicit builder: CfgBuilder): ExprResult =
    buildExprOr(expr, builder.undefined, rreq)


  private def buildStatements(stmts: Seq[Ast.Statement])(implicit builder: CfgBuilder): Unit = {
    def insertReportImpl(stmt: Ast.Statement, before: Boolean, code: String): Unit = {
      val line =
        if (before) code.substring(0, stmt.startIndex).count(_ == '\n')
        else code.substring(0, stmt.endIndex - 1).count(_ == '\n')
      builder.report(line, before)
    }
    val insertReport: (Ast.Statement, Boolean) => Unit = insertReports.value match {
      case Some(code) => insertReportImpl(_, _, code)
      case None => (_, _) => ()
    }

    var first = true
    for (stmt <- stmts) {
      if (first) {
        first = false
        insertReport(stmt, true)
      }
      transformStatement(stmt)
      insertReport(stmt, false)
    }
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |x = (x) => {
        |  return undefined
        |}
        |
        |x() + 1 + y.e.x()()
        |
        |debug.print(x)
        |
        |if (test) {
        |  x = {
        |     a: l + b,
        |     b: ha.u(3)
        |  }
        |} else {
        |  x = x()
        |}
        |""".stripMargin
    val cfg = transformScript(LangParser.parse(code))
    println(cfg.asmText())
  }
}
