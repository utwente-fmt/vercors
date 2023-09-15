package vct.col.ast.lang

import vct.col.ast.{SYCLNDRange, SYCLTNDRange, Type}
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLNDRangeImpl[G] { this: SYCLNDRange[G] =>
  override def t: Type[G] = SYCLTNDRange(dimensions.size)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_range") <> "<" <> Text(dimensions.size.toString) <> ">" <>
      "(" <> Text(dimensions.map(x => x.show).mkString(",")) <> ")")
}