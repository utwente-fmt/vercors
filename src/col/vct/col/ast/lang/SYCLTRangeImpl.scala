package vct.col.ast.lang

import vct.col.ast.SYCLTRange
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLTRangeImpl[G] { this: SYCLTRange[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::range") <> "<" <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::range"
}