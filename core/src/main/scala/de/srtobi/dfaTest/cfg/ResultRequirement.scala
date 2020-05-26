package de.srtobi.dfaTest
package cfg

import de.srtobi.dfaTest.dfa._

import scala.language.implicitConversions

abstract class ExprResult {
  def get: DfVarOrValue
  def pin(implicit builder: CfgBuilder): DfVarOrValue = builder.pin(get)
  def pinRegister(implicit builder: CfgBuilder): DfRegister = builder.pinToRegister(get)
}

object ExprResult {
  implicit def ResultTo(result: ExprResult): DfVarOrValue = result.get
}

abstract class ResultRequirement {
  def satisfy(result: DfVarOrValue, noop: Boolean = false)(implicit builder: CfgBuilder): ExprResult
  def satisfyUndefined(noop: Boolean = false)(implicit builder: CfgBuilder): ExprResult = satisfy(builder.undefined, noop)
  def satisfyNothing(noop: Boolean = false)(implicit builder: CfgBuilder): ExprResult = satisfy(builder.undefined, noop)

  def pin()(implicit builder: CfgBuilder): (DfVariable, ExprResult)
  def tryPin()(implicit builder: CfgBuilder): (Option[DfVariable], ExprResult)

  def derivePinned()(implicit builder: CfgBuilder): (ResultRequirement, ExprResult)

  def needsResult: Boolean
}

case object RequireNoResult extends ResultRequirement {
  private case object NoResult extends ExprResult {
    override def get: DfVarOrValue =
      throw new UnsupportedOperationException("Cannot get result. No result was requested")
  }

  override def satisfy(result: DfVarOrValue, noop: Boolean = true)(implicit builder: CfgBuilder): ExprResult = {
    if (noop) {
      builder.noop(result)
    }
    NoResult
  }

  override def pin()(implicit builder: CfgBuilder): (DfVariable, ExprResult) =
    throw new UnsupportedOperationException("Can't pin result, because no result is demanded.")

  override def tryPin()(implicit builder: CfgBuilder): (Option[DfVariable], ExprResult) =
    None -> NoResult

  override def derivePinned()(implicit builder: CfgBuilder): (ResultRequirement, ExprResult) =
    RequireNoResult -> NoResult

  override def needsResult: Boolean = false
}



final class RequireResultToProvidedSink(sink: DfVariable) extends ResultRequirement {
  case object SinkResult extends ExprResult {
    override def get: DfVarOrValue = sink
  }

  override def satisfy(result: DfVarOrValue, noop: Boolean)(implicit builder: CfgBuilder): ExprResult = {
    builder.mov(sink, result)
    SinkResult
  }

  override def pin()(implicit builder: CfgBuilder): (DfVariable, ExprResult) =
    sink -> SinkResult

  override def tryPin()(implicit builder: CfgBuilder): (Option[DfVariable], ExprResult) =
    Some(sink) -> SinkResult

  override def derivePinned()(implicit builder: CfgBuilder): (ResultRequirement, ExprResult) =
    this -> SinkResult

  override def needsResult: Boolean = true
}



sealed class RequireDirectResult extends ResultRequirement {
  import RequireDirectResult._
  override def satisfy(result: DfVarOrValue, noop: Boolean)(implicit builder: CfgBuilder): ExprResult = {
    DirectResult(result)
  }

  override def pin()(implicit builder: CfgBuilder): (DfVariable, ExprResult) = {
    val reg = builder.newRegister()
    reg -> DirectResult(reg)
  }

  override def tryPin()(implicit builder: CfgBuilder): (Option[DfVariable], ExprResult) = {
    val (reg, res) = pin()
    Some(reg) -> res
  }

  override def derivePinned()(implicit builder: CfgBuilder): (ResultRequirement, ExprResult) = {
    val reg = builder.newRegister()
    RequireResult(reg) -> DirectResult(reg)
  }

  override def needsResult: Boolean = true
}

object RequireDirectResult {
  private case class DirectResult(value: DfVarOrValue) extends ExprResult {
    override def get: DfVarOrValue = value
  }
}

object RequireResult extends RequireDirectResult {
  def apply(sink: DfVariable): RequireResultToProvidedSink = new RequireResultToProvidedSink(sink)

  object If {
    def apply(needsResult: Boolean): ResultRequirement = if (needsResult) RequireResult else RequireNoResult
  }
}