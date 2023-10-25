package vct.col.ast.lang

import vct.col.ast.CPPTArray
import vct.col.print.{Ctx, Doc, Group}

trait CPPTArrayImpl[G] { this: CPPTArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(innerType.show <> "[" <> Doc.args(size.toSeq) <> "]")
}

