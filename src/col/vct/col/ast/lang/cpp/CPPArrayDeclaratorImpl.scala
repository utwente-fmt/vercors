package vct.col.ast.lang.cpp

import vct.col.ast.CPPArrayDeclarator
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.CPPArrayDeclaratorOps

trait CPPArrayDeclaratorImpl[G] extends CPPArrayDeclaratorOps[G] { this: CPPArrayDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(inner.show <> "[" <> Doc.arg(Doc.stack(size.toSeq)) <> "]")
}