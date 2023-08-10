package vct.col.ast.lang

import vct.col.ast.CStructMemberDeclarator
import vct.col.print.{Ctx, Doc, Empty, Text}

trait CStructMemberDeclaratorImpl[G] { this: CStructMemberDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.spread(specs) <>
    (if(decls.isEmpty) Empty else Text(" ") <> Doc.fold(decls)(_ <> ", " <> _)) <> ";" // <> (if(decls.isEmpty) Empty else Doc.args(decls.get)) <> ";"
}