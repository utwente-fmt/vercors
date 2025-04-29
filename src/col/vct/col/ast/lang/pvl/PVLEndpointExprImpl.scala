package vct.col.ast.lang.pvl

import vct.col.ast.ops.PVLEndpointExprOps
import vct.col.ast.{
  Declaration,
  PVLEndpointExpr,
  PVLCommTargetEndpoint,
  PVLCommTargetRange,
  Type,
}
import vct.col.print._

trait PVLEndpointExprImpl[G] extends PVLEndpointExprOps[G] {
  this: PVLEndpointExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("(\\endpoint") <+> endpoint <> forallPart <> ";" <+> expr <> ")"

  def forallPart(implicit ctx: Ctx): Doc =
    bindings match {
      case Seq() => Empty
      case bindings => Text(", ∀") <> Doc.args(bindings.map(_.show))
    }

  def t: Type[G] = expr.t

  def declarations: Seq[Declaration[G]] =
    ((endpoint match {
      case PVLCommTargetRange(_, range) => Seq(range.binder)
      case _ => Seq()
    }) ++ bindings)
}
