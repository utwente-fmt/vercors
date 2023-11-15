package vct.col.ast.lang

import vct.col.ast.{SYCLReadOnlyAccess, SYCLTAccessMode, Type}
import vct.col.print.{Ctx, Doc, Text}

trait SYCLReadOnlyAccessImpl[G] { this: SYCLReadOnlyAccess[G] =>
  override def t: Type[G] = SYCLTAccessMode()

  override def layout(implicit ctx: Ctx): Doc = Text(name)

  val name = "sycl::read_only"
}