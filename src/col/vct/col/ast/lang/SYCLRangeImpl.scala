package vct.col.ast.lang

import vct.col.ast.{SYCLRange, SYCLTRange, Type}
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLRangeImpl[G] { this: SYCLRange[G] =>
  override def t: Type[G] = SYCLTRange(dimCount)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::range") <> "<" <> Text(dimCount.toString) <> ">" <>
      "(" <> Text(dimensions.mkString(",")) <> ")")
}