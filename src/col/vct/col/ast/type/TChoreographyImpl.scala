package vct.col.ast.`type`

import vct.col.ast.TChoreography
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TChoreographyOps

trait TChoreographyImpl[G] extends TChoreographyOps[G] {
  this: TChoreography[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(cls))
}
