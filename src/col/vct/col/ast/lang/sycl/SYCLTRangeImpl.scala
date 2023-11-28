package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTRange
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SYCLTRangeOps

trait SYCLTRangeImpl[G] extends SYCLTRangeOps[G] { this: SYCLTRange[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::range") <> "<" <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::range"
}