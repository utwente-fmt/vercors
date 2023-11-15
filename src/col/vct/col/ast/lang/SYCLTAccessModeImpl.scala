package vct.col.ast.lang

import vct.col.ast.SYCLTAccessMode
import vct.col.print.{Ctx, Doc, Text}

trait SYCLTAccessModeImpl[G] { this: SYCLTAccessMode[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("VERCORS::SYCL::ACCESS_MODE")

  override val namespacePath = ""
}