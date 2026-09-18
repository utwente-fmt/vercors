package vct.col.ast.lang.c

import vct.col.ast.CTFunction
import vct.col.ast.ops.CTFunctionOps
import vct.col.print.{Ctx, Doc, Group}

trait CTFunctionImpl[G] extends CTFunctionOps[G] {
  this: CTFunction[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(returnType.show <+> "function(" <> Doc.args(params) <> ")")
}
