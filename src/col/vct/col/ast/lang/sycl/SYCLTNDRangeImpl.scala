package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTNDRange
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SYCLTNDRangeOps

trait SYCLTNDRangeImpl[G] extends SYCLTNDRangeOps[G] { this: SYCLTNDRange[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_range") <> "<" <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::nd_range"
}