package vct.col.ast.lang

import vct.col.ast.SYCLTLocalAccessor
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLTLocalAccessorImpl[G] { this: SYCLTLocalAccessor[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::local_accessor") <> "<" <> typ <> ", " <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::local_accessor"
}