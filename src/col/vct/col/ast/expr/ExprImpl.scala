package vct.col.ast.expr

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{Expr, ProcessPar, Star, Type}
import vct.col.check.{CheckError, TypeError}
import vct.col.print._
import vct.col.typerules.CoercionUtils
import vct.col.ast.ops.ExprFamilyOps

trait ExprImpl[G] extends NodeFamilyImpl[G] with ExprFamilyOps[G] {
  this: Expr[G] =>
  def checkSubType(other: Type[G]): Seq[CheckError] =
    CoercionUtils.getCoercion(t, other) match {
      case Some(_) => Nil
      case None => Seq(TypeError(this, other))
    }

  def t: Type[G]

  private def unfold(
      node: Expr[G]
  )(matchFunc: PartialFunction[Expr[G], Seq[Expr[G]]]): Seq[Expr[G]] =
    matchFunc.lift(node) match {
      case Some(value) => value.flatMap(unfold(_)(matchFunc))
      case None => Seq(node)
    }

  def unfoldStar: Seq[Expr[G]] =
    unfold(this) { case Star(left, right) => Seq(left, right) }
  def unfoldProcessPar: Seq[Expr[G]] =
    unfold(this) { case ProcessPar(l, r) => Seq(l, r) }

  def precedence: Int = Precedence.UNKNOWN

  def bind(precedence: Int)(implicit ctx: Ctx): Doc =
    if (this.precedence >= precedence)
      show
    else
      Text("(") <> show <> ")"

  def assoc(other: Expr[_])(implicit ctx: Ctx): Doc = other.bind(precedence)
  def nassoc(other: Expr[_])(implicit ctx: Ctx): Doc =
    other.bind(precedence - 1)

  def lassoc(left: Expr[_], op: String, right: Expr[_])(
      implicit ctx: Ctx
  ): Doc = Group(assoc(left) <+> op <+/> nassoc(right))

  def rassoc(left: Expr[_], op: String, right: Expr[_])(
      implicit ctx: Ctx
  ): Doc = Group(nassoc(left) <+> op <+/> assoc(right))
}
