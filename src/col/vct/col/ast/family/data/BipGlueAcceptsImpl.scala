package vct.col.ast.family.data

import vct.col.ast.BipGlueAccepts
import vct.col.print._

trait BipGlueAcceptsImpl[G] {
  this: BipGlueAccepts[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(port)) <> ".accepts(" <>
      Doc.args(others.map(ctx.name).map(Text)) <> ");"
}
