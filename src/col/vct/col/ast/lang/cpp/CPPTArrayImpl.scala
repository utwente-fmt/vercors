package vct.col.ast.lang.cpp

import vct.col.ast.CPPTArray
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.CPPTArrayOps

trait CPPTArrayImpl[G] extends CPPTArrayOps[G] { this: CPPTArray[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(innerType.show <> "[" <> Doc.args(size.toSeq) <> "]")
}

