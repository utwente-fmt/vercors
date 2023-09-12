package vct.col.ast.lang

import vct.col.ast.{SYCLHandler, SYCLTHandler, Type}
import vct.col.print.{Ctx, Doc, Text}

trait SYCLHandlerImpl[G] { this: SYCLHandler[G] =>
  override def t: Type[G] = SYCLTHandler()

  override def layout(implicit ctx: Ctx): Doc = Text("sycl::handler()")
}