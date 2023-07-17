package vct.col.ast.`type`

import vct.col.ast.TVeyMontChannel
import vct.col.print.{Ctx, Doc, Text}

trait TVeyMontChannelImpl[G] { this: TVeyMontChannel[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(this.channelType)
}