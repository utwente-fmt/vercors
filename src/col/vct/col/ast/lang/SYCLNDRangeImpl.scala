package vct.col.ast.lang

import vct.col.ast.{SYCLNDRange, SYCLTNDRange, Type}
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLNDRangeImpl[G] { this: SYCLNDRange[G] =>
  override def t: Type[G] = SYCLTNDRange(dimCount)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_range") <> "<" <> Text(dimCount.toString) <> ">" <>
      "(" <> Text(dimensions.map(x => x.show).mkString(",")) <> ")")
}