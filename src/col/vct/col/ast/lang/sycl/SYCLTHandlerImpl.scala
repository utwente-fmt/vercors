package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTHandler
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.SYCLTHandlerOps

trait SYCLTHandlerImpl[G] extends SYCLTHandlerOps[G] { this: SYCLTHandler[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::handler")

  override val namespacePath = "sycl::handler"
}