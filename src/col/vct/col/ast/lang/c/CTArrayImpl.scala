package vct.col.ast.lang.c

import vct.col.ast.CTArray
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.CTArrayOps

trait CTArrayImpl[G] extends CTArrayOps[G] { this: CTArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(innerType.show <> "[" <> Doc.args(size.toSeq) <> "]")
}

