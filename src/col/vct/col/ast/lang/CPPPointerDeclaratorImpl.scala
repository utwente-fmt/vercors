package vct.col.ast.lang

import vct.col.ast.CPPPointerDeclarator
import vct.col.print.{Ctx, Doc}

trait CPPPointerDeclaratorImpl[G] {
  this: CPPPointerDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    inner.show <> Doc.fold(pointers)(_ <> _)
}
