package vct.col.ast.family.data

import vct.col.ast.BipGlueDataWire
import vct.col.print._
import vct.col.ast.ops.{BipGlueDataWireOps, BipGlueDataWireFamilyOps}

trait BipGlueDataWireImpl[G] extends BipGlueDataWireOps[G] with BipGlueDataWireFamilyOps[G] { this: BipGlueDataWire[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(dataOut)) <> ".to(" <> Doc.arg(Text(ctx.name(dataIn))) <> ");"
}
