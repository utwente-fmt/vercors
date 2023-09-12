package vct.col.ast.lang

import vct.col.ast.SYCLTEvent
import vct.col.print.{Ctx, Doc, Text}

trait SYCLTEventImpl[G] { this: SYCLTEvent[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::event")
}