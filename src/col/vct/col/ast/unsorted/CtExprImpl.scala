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

  // TODO (RR): I think this is wrong, but keeping it around for a few minutes
  override def t: TClass[G] = inner.ref.decl.t
//    inner match {
//      case _: CommTargetEndpoint[G] | _: CommTargetIndex[G] => inner.ref.decl.t
//      case _: CommTargetRange[G] => TSeq(inner.ref.decl.t)
//    }

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
