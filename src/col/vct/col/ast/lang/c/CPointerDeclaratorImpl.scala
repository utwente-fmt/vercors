package vct.col.ast.lang.c

import vct.col.ast.CPointerDeclarator
import vct.col.ast.ops.CPointerDeclaratorOps
import vct.col.print.{Ctx, Doc}

trait CPointerDeclaratorImpl[G] extends CPointerDeclaratorOps[G] {
  this: CPointerDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.fold(pointers)(_ <> _) <> inner.show
}
