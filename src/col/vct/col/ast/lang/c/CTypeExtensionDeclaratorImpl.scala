package vct.col.ast.lang.c

import vct.col.ast.CTypeExtensionDeclarator
import vct.col.ast.ops.CTypeExtensionDeclaratorOps
import vct.col.print.{Ctx, Doc}

trait CTypeExtensionDeclaratorImpl[G] extends CTypeExtensionDeclaratorOps[G] { this: CTypeExtensionDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    inner.show <> Doc.fold(extensions)(_ <+> _)
}