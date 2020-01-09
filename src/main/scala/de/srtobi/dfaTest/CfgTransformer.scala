package de.srtobi.dfaTest

import de.srtobi.dfaTest.cfg._

object CfgTransformer {
  def transformScript(script: Ast.Script): ControlFlowGraph = {
    implicit val builder: CfgBuilder = new CfgBuilder
    buildStatements(script.main)
    builder.end()
    builder.build()
  }

  object DebugExpr {
    val debugIdentifier: Ast.Identifier = Ast.Identifier("debug")

    def unapply(expr: Ast.Expression): Option[Seq[CfgBuilder => Debug.Check]] = Option(expr match {
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
            val rhs = transformExpr(rhsExpr, RequireResult)(cfgBuilder).get
            Debug.Is(lhs, rhs, LangPrinter.print(expr))
          case _ =>
            throw new Exception("")
        })
      case _ =>
        null
    })
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
        throw new Exception("Cannot use 'is' operator in exception")
      }
      val left = transformExpr(leftExpr, RequireResult)
      val right = transformExpr(rightExpr, RequireResult)
      val (maybeRet, result) = rreq.tryPin()
      val ret = maybeRet.getOrElse(builder.newRegister())
      builder.binaryOp(ret, left, op, right)
      result
  }

  def transformStatement(statement: Ast.Statement)(implicit builder: CfgBuilder): Unit = statement match {
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


  private def buildExprOr(expr: Option[Ast.Expression],
                          result: => DfVarOrValue,
                          rreq: ResultRequirement = RequireResult)(implicit builder: CfgBuilder): ExprResult =
    expr.map(transformExpr(_, rreq)).getOrElse(rreq.satisfy(result))

  private def buildExprOrUndefined(expr: Option[Ast.Expression],
                     rreq: ResultRequirement = RequireResult)(implicit builder: CfgBuilder): ExprResult =
    buildExprOr(expr, builder.undefined, rreq)


  private def buildStatements(stmts: Seq[Ast.Statement])(implicit builder: CfgBuilder): Unit =
    stmts.foreach(transformStatement)

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
