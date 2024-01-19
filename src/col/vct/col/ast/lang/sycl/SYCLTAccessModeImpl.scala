package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTAccessMode
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.SYCLTAccessModeOps

trait SYCLTAccessModeImpl[G] extends SYCLTAccessModeOps[G] { this: SYCLTAccessMode[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("VERCORS::SYCL::ACCESS_MODE")

  override val namespacePath = ""
}