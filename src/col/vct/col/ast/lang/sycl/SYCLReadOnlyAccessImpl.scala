package vct.col.ast.lang.sycl

import vct.col.ast.{SYCLReadOnlyAccess, SYCLTAccessMode, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.SYCLReadOnlyAccessOps

trait SYCLReadOnlyAccessImpl[G] extends SYCLReadOnlyAccessOps[G] { this: SYCLReadOnlyAccess[G] =>
  override def t: Type[G] = SYCLTAccessMode()

  override def layout(implicit ctx: Ctx): Doc = Text(name)

  val name = "sycl::read_only"
}