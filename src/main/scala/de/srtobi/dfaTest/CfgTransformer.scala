package de.srtobi.dfaTest

import de.srtobi.dfaTest.cfg._

object CfgTransformer {
  def transformScript(script: Ast.Script): ControlFlowGraph = {
    implicit val builder: CfgBuilder = new CfgBuilder
    buildStatements(script.main)
    builder.end()
    builder.build()
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

    case Ast.PropertyAccess(base, prop) =>
      val baseEntity = transformExpr(base, RequireResult).get
      val (maybePin, result) = rreq.tryPin()
      maybePin.foreach(builder.read(_, baseEntity, prop))
      result

    case Ast.Call(func, args) =>
      val funcEntity = transformExpr(func, RequireResult)
      val argEntities = args.map(transformExpr(_, RequireResult).get)
      val (maybeRet, result) = rreq.tryPin()
      builder.call(maybeRet, funcEntity, argEntities)
      result

    case f@Ast.Function(paramNames, block) =>
      val subBuilder = builder.createSubBuilder()
      val paramEntity = paramNames.map(builder.resolveVariable).map(new DfConcreteLambdaRef.Parameter(_))
      buildStatements(block)(subBuilder)
      subBuilder.end()
      val cfg = subBuilder.build()
      val lambda = new DfConcreteLambdaRef(f, paramEntity, cfg)
      rreq.satisfy(lambda)

    case Ast.Operator(op, leftExpr, rightExpr) =>
      val left = transformExpr(leftExpr, RequireResult)
      val right = transformExpr(rightExpr, RequireResult)
      val (maybeRet, result) = rreq.tryPin()
      val ret = maybeRet.getOrElse(builder.newRegister())
      builder.binaryOp(ret, left, op, right)
      result
  }

  def transformStatement(statement: Ast.Statement)(implicit builder: CfgBuilder): Unit = statement match {
    case Ast.ExpressionStmt(expr) =>
      transformExpr(expr, RequireNoResult)

    case Ast.IfStmt(condExpr, thenExpr, elseExpr) =>
      val hasElse = elseExpr.isDefined
      val endLabel = builder.createLabel("endIf")
      val elseLabel = if (hasElse) builder.createLabel("else") else endLabel

      val condEntity = transformExpr(condExpr, RequireResult)
      builder.jumpIfFalse(condEntity, elseLabel)
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
      val baseEntity = transformExpr(baseExpr, RequireResult).get
      val entity = transformExpr(expr, RequireResult).get

      builder.write(baseEntity, property, entity)

    case Ast.AssignmentStmt(Ast.Identifier(variable), expr) =>
      transformExpr(expr, RequireResult(builder.resolveVariable(variable)))

    case Ast.AssignmentStmt(_, _) =>
      throw new Exception("Can only assign to variables and properties")
  }


  private def buildExprOr(expr: Option[Ast.Expression],
                          result: => DfEntity,
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
