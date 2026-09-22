package vct.col.ast.`type`

import vct.col.ast.TUnion
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.TUnionOps

trait TUnionImpl[G] extends TUnionOps[G] {
  this: TUnion[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      ctx.syntax match {
        case Ctx.Isar => Doc.fold(types) (_ <+> "+" <+> _)
        case _ => Doc.fold(types)(_ <+> "|" <+> _)
      }
    )
}
