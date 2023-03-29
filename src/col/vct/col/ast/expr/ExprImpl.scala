package vct.col.ast.expr

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{Expr, ProcessPar, Star, Type}
import vct.col.check.{CheckError, TypeError}
import vct.col.typerules.CoercionUtils

trait ExprImpl[G] extends NodeFamilyImpl[G] { this: Expr[G] =>
  def checkSubType(other: Type[G]): Seq[CheckError] =
    CoercionUtils.getCoercion(t, other) match {
      case Some(_) => Nil
      case None => Seq(TypeError(this, other))
    }

  def t: Type[G]

  private def unfold(node: Expr[G])(matchFunc: PartialFunction[Expr[G], Seq[Expr[G]]]): Seq[Expr[G]] =
    matchFunc.lift(node) match {
      case Some(value) => value.flatMap(unfold(_)(matchFunc))
      case None => Seq(node)
    }

  def unfoldStar: Seq[Expr[G]] = unfold(this) { case Star(left, right) => Seq(left, right) }
  def unfoldProcessPar: Seq[Expr[G]] = unfold(this) { case ProcessPar(l, r) => Seq(l, r) }
}