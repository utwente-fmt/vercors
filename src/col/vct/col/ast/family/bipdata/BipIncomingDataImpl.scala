package vct.col.ast.family.bipdata

import vct.col.ast.BipIncomingData
import vct.col.print._
import vct.col.ast.ops.BipIncomingDataOps

trait BipIncomingDataImpl[G] extends BipIncomingDataOps[G] { this: BipIncomingData[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("/* bipIncomingData") <+> t <+> ctx.name(this) <+> "*/"
}
