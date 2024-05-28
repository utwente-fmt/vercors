package vct.col.ast.family.data

import vct.col.ast.BipGlueAccepts
import vct.col.print._
import vct.col.ast.ops.{BipGlueAcceptsOps, BipGlueAcceptsFamilyOps}

trait BipGlueAcceptsImpl[G]
    extends BipGlueAcceptsOps[G] with BipGlueAcceptsFamilyOps[G] {
  this: BipGlueAccepts[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(port)) <> ".accepts(" <>
      Doc.args(others.map(ctx.name).map(Text)) <> ");"
}
