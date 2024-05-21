package vct.col.ast.lang.sycl

import vct.col.ast.{SYCLNDRange, SYCLTNDRange, SYCLTRange, Type}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SYCLNDRangeOps

trait SYCLNDRangeImpl[G] extends SYCLNDRangeOps[G] { this: SYCLNDRange[G] =>
  override def t: Type[G] = globalSize.t match {
    case SYCLTRange(dimCount) => SYCLTNDRange(dimCount)
    case _ => ??? // All other types are not allowed
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_range") <> "<" <> Text(this.t.asInstanceOf[SYCLTNDRange[G]].dimCount.toString) <> ">" <>
      "(" <> globalSize.show <> ", " <> localSize.show <> ")")
}