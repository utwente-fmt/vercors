package vct.col.ast.lang

import vct.col.ast.{SYCLEvent, SYCLTEvent, Type}
import vct.col.print.{Ctx, Doc, Text}

trait SYCLEventImpl[G] { this: SYCLEvent[G] =>
  override def t: Type[G] = SYCLTEvent()

  override def layout(implicit ctx: Ctx): Doc = Text("sycl::event()")
}