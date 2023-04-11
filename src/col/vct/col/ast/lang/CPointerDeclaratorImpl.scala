package vct.col.ast.lang

import vct.col.ast.CPointerDeclarator
import vct.col.print.{Ctx, Doc}

trait CPointerDeclaratorImpl[G] { this: CPointerDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    inner.show <> Doc.fold(pointers)(_ <> _)
}