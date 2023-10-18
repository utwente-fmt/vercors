package vct.col.ast.lang

import vct.col.ast.CPPArrayDeclarator
import vct.col.print.{Ctx, Doc, Group}

trait CPPArrayDeclaratorImpl[G] {
  this: CPPArrayDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(inner.show <> "[" <> Doc.arg(Doc.stack(size.toSeq)) <> "]")
}
