package vct.col.ast.family.data

import vct.col.ast.BipGlueRequires
import vct.col.print._
import vct.col.ast.ops.{BipGlueRequiresOps, BipGlueRequiresFamilyOps}

trait BipGlueRequiresImpl[G] extends BipGlueRequiresOps[G] with BipGlueRequiresFamilyOps[G] { this: BipGlueRequires[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(port)) <> ".requires(" <> Doc.args(others.map(ctx.name).map(Text)) <> ");"
}
