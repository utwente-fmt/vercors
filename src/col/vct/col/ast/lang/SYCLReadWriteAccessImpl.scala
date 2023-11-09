package vct.col.ast.lang

import vct.col.ast.{SYCLReadWriteAccess, SYCLTAccessMode, Type}
import vct.col.print.{Ctx, Doc, Text}

trait SYCLReadWriteAccessImpl[G] { this: SYCLReadWriteAccess[G] =>
  override def t: Type[G] = SYCLTAccessMode()

  override def layout(implicit ctx: Ctx): Doc = Text(name)

  val name = "sycl::read_write"
}