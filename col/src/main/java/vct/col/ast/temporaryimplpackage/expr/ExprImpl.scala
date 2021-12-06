package vct.col.ast.temporaryimplpackage.expr

import vct.col.ast.temporaryimplpackage.node.NodeFamilyImpl
import vct.col.ast.{Expr, ProcessPar, Star, Type}
import vct.col.check.{CheckError, TypeError}
import vct.col.coerce.Coercion

trait ExprImpl extends NodeFamilyImpl { this: Expr =>
  def checkSubType(other: Type): Seq[CheckError] =
    Coercion.getCoercion(t, other) match {
      case Some(_) => Nil
      case None => Seq(TypeError(this, other))
    }

  def t: Type

  private def unfold(node: Expr)(matchFunc: PartialFunction[Expr, Seq[Expr]]): Seq[Expr] =
    matchFunc.lift(node) match {
      case Some(value) => value.flatMap(unfold(_)(matchFunc))
      case None => Seq(node)
    }

  def unfoldStar: Seq[Expr] = unfold(this) { case Star(left, right) => Seq(left, right) }
  def unfoldProcessPar: Seq[Expr] = unfold(this) { case ProcessPar(l, r) => Seq(l, r) }
}