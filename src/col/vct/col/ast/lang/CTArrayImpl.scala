package vct.col.ast.lang

import vct.col.ast.CTArray
import vct.col.print.{Ctx, Doc, Group}

trait CTArrayImpl[G] { this: CTArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(innerType.show <> "[" <> Doc.args(size.toSeq) <> "]")
}

