package vct.col.ast.unsorted

import vct.col.ast.{
  CommTargetEndpoint,
  CommTargetIndex,
  CommTargetRange,
  CtExpr,
  TClass,
  TSeq,
  Type,
}
import vct.col.ast.ops.CtExprOps
import vct.col.print._
import vct.col.util.AstMatchHelpers.{EndpointIndex, EndpointName, EndpointRange}

trait CtExprImpl[G] extends CtExprOps[G] {
  this: CtExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc = inner.show
  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] =
    inner match {
      case _: CommTargetEndpoint[G] | _: CommTargetIndex[G] =>
        inner.ref.decl.singleType
      case _: CommTargetRange[G] => inner.ref.decl.rangeType
    }

  def isName: Boolean =
    this match {
      case EndpointName(_) => true
      case _ => false
    }

  def isRange: Boolean =
    this match {
      case EndpointRange(_, _) => true
      case _ => false
    }

  def isIndex: Boolean =
    this match {
      case EndpointIndex(_, _) => true
      case _ => false
    }
}
