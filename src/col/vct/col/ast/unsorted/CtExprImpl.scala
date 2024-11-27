package vct.col.ast.unsorted

import vct.col.ast.{CtExpr, Type}
import vct.col.ast.ops.CtExprOps
import vct.col.print._
import vct.col.util.AstMatchHelpers.{EndpointIndex, EndpointName, EndpointRange}

trait CtExprImpl[G] extends CtExprOps[G] {
  this: CtExpr[G] =>
//  override def layout(implicit ctx: Ctx): Doc = inner.layout

  override def t: Type[G] = inner.endpoint.t

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
