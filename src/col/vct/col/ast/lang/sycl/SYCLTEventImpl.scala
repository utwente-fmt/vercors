package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTEvent
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.SYCLTEventOps

trait SYCLTEventImpl[G] extends SYCLTEventOps[G] { this: SYCLTEvent[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::event")

  override val namespacePath = "sycl::event"
}