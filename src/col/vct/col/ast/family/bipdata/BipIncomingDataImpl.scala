package vct.col.ast.family.bipdata

import vct.col.ast.BipIncomingData
import vct.col.print._

trait BipIncomingDataImpl[G] {
  this: BipIncomingData[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("/* bipIncomingData") <+> t <+> ctx.name(this) <+> "*/"
}
