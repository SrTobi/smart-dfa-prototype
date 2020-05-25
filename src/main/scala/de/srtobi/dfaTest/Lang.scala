package de.srtobi.dfaTest

import fastparse.Parsed

import scala.annotation.tailrec


object Ast {

  sealed abstract class Node {
    val id: UId = UId("ast-node")
  }
  sealed abstract class Expression extends Node
  case object UndefinedLiteral extends Expression
  case class StringLiteral(value: String) extends Expression
  case class BooleanLit(value: Boolean) extends Expression
  case class NumberLit(value: Int) extends Expression
  case class Identifier(name: String) extends Expression
  case class Union(elements: Seq[Expression]) extends Expression
  case class Object(properties: Seq[Property]) extends Expression
  case class Property(name: String, init: Expression) extends Node
  case class Operator(op: String, left: Expression, right: Expression) extends Expression
  case class Function(params: Seq[String], block: Block) extends Expression
  case class PropertyAccess(base: Expression, property: String) extends Expression
  case class Call(function: Expression, args: Seq[Expression]) extends Expression

  sealed abstract class Statement(val startIndex: Int, val endIndex: Int) extends Node
  type Block = Seq[Statement]
  case class ExpressionStmt(expr: Expression)(_index: Int, _endIndex: Int) extends Statement(_index, _endIndex)
  case class IfStmt(condition: Expression, success: Block, fail: Option[Block])(_index: Int, _endIndex: Int) extends Statement(_index, _endIndex)
  case class ReturnStmt(expression: Option[Expression])(_index: Int, _endIndex: Int) extends Statement(_index, _endIndex)
  case class AssignmentStmt(target: Expression, expression: Expression)(_index: Int, _endIndex: Int) extends Statement(_index, _endIndex)

  case class Script(main: Block) extends Node
}


object LangTokens {
  import fastparse._
  import NoWhitespace._

  // from # 11.6
  // implementation inspired by http://www.scala-sbt.org/0.12.4/sxr/Parsers.scala.html#326954
  private def unicodeIdStart[_: P] = CharPred(_.isLetter)
  private def unicodeIdContinue[_: P] = CharPred(c => c.isLetterOrDigit)

  // # 10.1
  def sourceCharacter[_: P]: P[Unit] = AnyChar

  // # 11.2
  def whiteSpace[_: P]: P[Unit] = CharIn(" ", "\t", "\u000B", "\u000C", "\u00A0").opaque("ws")

  // # 11.3
  def lineTerminator[_: P]: P[Unit] = CharIn("\n", "\r", "\u2028", "\u2029").opaque("line-terminator")
  def lineTerminatorSequence[_: P]: P[Unit] = P(StringIn("\n", "\r\n", "\r", "\u2028", "\u2029")).opaque("\\n")

  // # 11.4
  def singleLineComment[_: P]: P[Unit] = "//" ~/ (!lineTerminator ~ sourceCharacter).rep
  private def commentEnd[_: P]: P[Unit] = "*/"
  private def noCommentEnd[_: P]: P[Unit] = P(!commentEnd ~ AnyChar)
  def multiLineComment[_: P]: P[Boolean] =
    NoCut(P("/*" ~/
      (!lineTerminator ~/ noCommentEnd).rep ~/
      lineTerminator.map(_ => true).? ~/
      noCommentEnd.rep ~/
      commentEnd
    ).opaque("multiline-comment")).map(_.getOrElse(false))
  def comment[_: P]: P[Unit] = P(multiLineComment.map(_ => ()) | singleLineComment).opaque("comment")
  def inlineComment[_: P]: P[Unit] = multiLineComment.filter(!_).map(_ => ())

  // # 11.6.2
  val futureReservedWord: Seq[String] = Seq("enum")
  val keyword: Seq[String] = Seq("if", "var", "else", "undefined", "return")
  val reservedWord: Seq[String] = keyword ++ Seq("null", "true", "false") ++ futureReservedWord

  // # 11.6
  def identifierStart[_: P]: P[Unit] = P(unicodeIdStart | "$" | "_")
  def identifierPart[_: P]: P[Unit] = P(unicodeIdContinue | "$" | "_")
  def identifierName[_: P]: P[String] = P(identifierStart ~ identifierPart.rep).!.filter(!reservedWord.contains(_))

  // # 11.8.1 / 11.8.2
  def nullLiteral[_: P]: P[Unit] = P("null")
  def booleanLiteral[_: P]: P[Boolean] = P("true").map(_ => true) | P("false").map(_ => false)

  // # 11.8.3
  def decimalDigit[_: P]: P[Unit] = CharIn("0123456789")
  def nonZeroDigit[_: P]: P[Unit] = CharIn("123456789")
  def decimalDigits[_: P]: P[Unit] = decimalDigit.rep(1)
  def decimalIntegerLiteral[_: P]: P[Int] = P("0" | nonZeroDigit ~ decimalDigits.?).!.map(_.toInt)

  private def lineContinuation[_: P]: P[Unit] = "\\" ~ lineTerminatorSequence
  private def doubleStringCharacter[_: P]: P[Unit] = !("\"" | "\\" | lineTerminator) ~ sourceCharacter | lineContinuation
  private def singleStringCharacter[_: P]: P[Unit] = !("\'" | "\\" | lineTerminator) ~ sourceCharacter | lineContinuation
  def stringLiteral[_: P]: P[String] = "\"" ~/ doubleStringCharacter.rep.! ~ "\"" | "\'" ~/ singleStringCharacter.rep.! ~ "\'"

  //val commonToken: UnitP = identifierName
  def ws[_: P]: P[Unit] = P(whiteSpace | lineTerminatorSequence | comment).opaque("ws")
  def wsWithLineTerminator[_: P]: P[Unit] = P(lineTerminatorSequence | multiLineComment.filter(a => a).map(_ => ()) | singleLineComment).opaque("line-terminating")
  def noLineTerminator[_: P]: P[Unit] = P(whiteSpace | inlineComment).opaque("no-line-terminator")
}

object LangParser {
  def instanceWithIndex[T](input: (Int, (Int, Int) => T, Int)): T = {
    val (index, f, endIndex) = input
    f(index, endIndex)
  }

  import LangTokens._
  import fastparse._
  import ScalaWhitespace._

  def property[_: P]: P[Ast.Property] = P(
    identifierName ~/ ":" ~/ expression
  ).map((Ast.Property.apply _).tupled)

  def blockOrReturnExpression[_:P]: P[Ast.Block] = P(
    block
      | (Index ~~ expression ~~ Index).map { case (index, e, endIndex) => Seq(Ast.ReturnStmt(Some(e))(index, endIndex)) }
  )

  def primaryExpression[_: P]: P[Ast.Expression] = P(
    P("undefined").map(_ => Ast.UndefinedLiteral) |
      booleanLiteral.map(Ast.BooleanLit) |
      decimalIntegerLiteral.map(Ast.NumberLit) |
      identifierName.map(Ast.Identifier) |
      stringLiteral.map(Ast.StringLiteral) |
      ("(" ~ identifierName.rep(sep=",") ~ ")" ~ "=>" ~ blockOrReturnExpression).map((Ast.Function.apply _).tupled) |
      ("(" ~/ expression ~ ")") |
      ("{" ~/ property.rep(sep=",") ~ "}").map(Ast.Object) |
      ("[" ~/ primaryExpression.rep(sep="|") ~ "]").map(Ast.Union.apply)
  )

  private def makeInner[_: P](left: Ast.Expression): P[Ast.Expression] = P(
    P(Pass ~ "(" ~/ expression.rep(sep=",") ~ ")").map(args => Ast.Call(left, args)) |
      P(Pass ~ "." ~/ identifierName).map(Ast.PropertyAccess(left, _))
  ).?.flatMapX(_.map(makeInner).getOrElse(Pass.map(_ => left)))

  def innerExpression[_: P]: P[Ast.Expression] = P(
    primaryExpression.flatMapX(makeInner)
  )

  val debugIsOperator: String = "is"

  private val precedence = Map(
    "+" -> 5,
    "-" -> 5,
    "==" -> 1,
    "!=" -> 1,
    debugIsOperator -> 0,
  )

  @tailrec
  private def precedenceClimbDown(minPrec: Int, lhs: Ast.Expression, right: Seq[(String, Ast.Expression)]): (Ast.Expression, Seq[(String, Ast.Expression)]) = {
    if (right.isEmpty) return lhs -> right
    val (lookAheadOp, _) +: _ = right
    val prec = precedence(lookAheadOp)
    if (prec > minPrec) {
      val (rhs2, rest2) = precedenceClimbUp(prec, lhs, right)
      precedenceClimbDown(minPrec, rhs2, rest2)
    } else lhs -> right
  }

  @tailrec
  private def precedenceClimbUp(minPrec: Int, lhs: Ast.Expression, right: Seq[(String, Ast.Expression)]): (Ast.Expression, Seq[(String, Ast.Expression)]) = {
    if (right.isEmpty) return lhs -> right
    val (op, rhs) +: rest = right
    val prec = precedence(op)
    if (prec >= minPrec) {
      val (rhs2, rest2) = precedenceClimbDown(prec, rhs, rest)
      precedenceClimbUp(minPrec, Ast.Operator(op, lhs, rhs2), rest2)
    } else lhs -> right
  }

  def expression[_: P]: P[Ast.Expression] = P(
    (innerExpression ~~ (Pass ~ ("+" | "-" | "==" | "!=" | debugIsOperator).! ~ innerExpression).repX).map {
      case (first, tail) =>
        val (result, Seq()) = precedenceClimbUp(0, first, tail)
        result
    }
  )

  def blockOrStatement[_: P]: P[Ast.Block] = P(
    block
      | P(";").map(_ => Seq())
      | statement.map(Seq(_))
  )

  def exprEnd[_: P]: P[Unit] = P(noLineTerminator.repX ~~ (&(End) | &("}") | &("else") | CharIn(";\n")))

  def statements[_: P]: P[Seq[Ast.Statement]] = P((CharIn(";").map(_ => None) | statement.map(Some(_))).rep.map(_.flatten))
  def block[_: P]: P[Seq[Ast.Statement]] = P("{" ~ statements ~ "}")
  def statement[_: P]: P[Ast.Statement] = P( Index ~~ (
    ("if" ~/ "(" ~/ expression ~ ")" ~/ blockOrStatement ~/ ("else" ~/ blockOrStatement).?).map((Ast.IfStmt.apply _).tupled) |
      ("return" ~/ expression.?).map(e => Ast.ReturnStmt(e)(_, _)) |
      (NoCut(expression) ~ "=" ~/ expression ~~ exprEnd).map((Ast.AssignmentStmt.apply _).tupled) |
      (expression ~~ exprEnd).map(e => Ast.ExpressionStmt(e)(_, _))
    ) ~~ Index
  ).map(instanceWithIndex)

  def script[_: P]: P[Ast.Script] = P(Pass ~ statements ~ End).map(Ast.Script)

  def parse(code: String): Ast.Script = {
    fastparse.parse(code, LangParser.script(_)) match {
      case Parsed.Success(ast, _) =>
        ast
      case f@Parsed.Failure(_, _, extra) =>
        println(f)
        println(extra.trace().longAggregateMsg)
        throw new Exception("Failed to compile")
    }
  }
}

object LangPrinter {
  import Ast._

  def print(node: Node): String = printNode(node, "")

  private def printNode(node: Node, indent: String): String = {
    def p(node: Node) = printNode(node, indent)
    node match {
      case Script(main) => printBlk(main, indent, root = true)
      case IfStmt(cond, success, Some(fail)) => indent + s"if (${p(cond)}) ${printBlk(success, indent)} else ${printBlk(fail, indent)}"
      case IfStmt(cond, success, None) => indent + s"if (${p(cond)}) ${printBlk(success, indent)}"
      case ReturnStmt(expr) => indent + s"return ${expr.map(p).getOrElse("")}"
      case AssignmentStmt(target, init) => indent + s"${p(target)} = ${p(init)}"
      case Object(properties) => properties.map(p).mkString("@{", ", ", "}")
      case Property(name, init) => s"$name: ${p(init)}"
      case Identifier(id) => id
      case ExpressionStmt(e) => indent + p(e);
      case StringLiteral(str) => "\"" + str + "\""
      case BooleanLit(bool) => bool.toString
      case NumberLit(num) => num.toString
      case Function(args, body) => args.mkString("$(", ",", ")") + printBlk(body, indent)
      case Call(base, args) => p(base) + args.map(p).mkString("(", ",", ")")
      case Operator(op, left, right) => s"(${p(left)} $op ${p(right)})"
      case PropertyAccess(base, property) => s"${p(base)}.$property"
      case Union(elements) => elements.map(p).mkString("[", " | ", "]")
      case UndefinedLiteral => "undefined"
    }
  }

  private def printBlk(block: Block, indent: String, root: Boolean = false): String = block
    .map(printNode(_, indent + (if(root) "" else "  "))) match {
    case l if root => l.mkString(";\n")
    case l => l.mkString("{\n", ";\n", "\n" + indent + "}")
  }

}


object LangTests {

  def main(args: Array[String]): Unit = {
    val code =
      """
        |var x = (x) => {
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
        |}
        |""".stripMargin
    fastparse.parse(code, LangParser.script(_)) match {
      case Parsed.Success(ast, _) =>
        println(ast)
        println(LangPrinter.print(ast))
      case f@Parsed.Failure(_, _, extra) =>
        println(f)
        println(extra.trace().longAggregateMsg)
    }
  }
}