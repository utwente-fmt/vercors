package vct.col.ast.lang.c

import vct.col.ast.CStructMemberDeclarator
import vct.col.ast.ops.{
  CStructMemberDeclaratorFamilyOps,
  CStructMemberDeclaratorOps,
}
import vct.col.print.{Ctx, Doc, Empty, Text}

trait CStructMemberDeclaratorImpl[G]
    extends CStructMemberDeclaratorOps[G]
    with CStructMemberDeclaratorFamilyOps[G] {
  this: CStructMemberDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.spread(specs) <>
      (if (decls.isEmpty)
         Empty
       else
         Text(" ") <> Doc.fold(decls)(_ <> ", " <> _)) <> ";"
}
