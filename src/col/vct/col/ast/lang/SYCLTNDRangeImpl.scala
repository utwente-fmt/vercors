package vct.col.ast.lang

import vct.col.ast.SYCLTNDRange
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLTNDRangeImpl[G] { this: SYCLTNDRange[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_range") <> "<" <> Text(dimCount.toString) <> ">")
}