package vct.col.ast.lang.sycl

import vct.col.ast.{SYCLReadWriteAccess, SYCLTAccessMode, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.SYCLReadWriteAccessOps

trait SYCLReadWriteAccessImpl[G] extends SYCLReadWriteAccessOps[G] { this: SYCLReadWriteAccess[G] =>
  override def t: Type[G] = SYCLTAccessMode()

  override def layout(implicit ctx: Ctx): Doc = Text(name)

  val name = "sycl::read_write"
}