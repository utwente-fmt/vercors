package vct.col.ast.lang.sycl

import vct.col.ast.SYCLClassDefName
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SYCLClassDefNameOps

trait SYCLClassDefNameImpl[G] extends SYCLClassDefNameOps[G] { this: SYCLClassDefName[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text(name) <>
      (if (genericArgs.nonEmpty) (Text("<") <> Doc.args(genericArgs) <> Text(">")) else Text("")))
}