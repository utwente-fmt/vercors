package vct.col.ast.lang

import vct.col.ast.SYCLTHandler
import vct.col.print.{Ctx, Doc, Text}

trait SYCLTHandlerImpl[G] { this: SYCLTHandler[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::handler")

  override val namespacePath = "sycl::handler"
}