package vct.col.ast.family.data

import vct.col.ast.BipGlueDataWire
import vct.col.print._

trait BipGlueDataWireImpl[G] { this: BipGlueDataWire[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(dataOut)) <> ".to(" <> Doc.arg(Text(ctx.name(dataIn))) <> ");"
}
