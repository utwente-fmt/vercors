package vct.col.ast.lang

import vct.col.ast.CArrayDeclarator
import vct.col.print.{Ctx, Doc, Group}

trait CArrayDeclaratorImpl[G] {
  this: CArrayDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      inner.show <> "[" <> Doc.arg(Doc.stack(qualifiers ++ size.toSeq)) <> "]"
    )
}
