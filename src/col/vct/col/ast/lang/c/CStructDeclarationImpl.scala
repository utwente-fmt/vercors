package vct.col.ast.lang.c

import vct.col.ast.CStructDeclaration
import vct.col.ast.ops.CStructDeclarationOps
import vct.col.print.{Ctx, Doc, Empty, Text}

trait CStructDeclarationImpl[G] extends CStructDeclarationOps[G] { this: CStructDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Doc.stack(Seq(
      Text("struct") <+>
      (if (name.isEmpty) Empty else Text(name.get)) <+>
        "{" <>> Doc.stack(decl) <+/> "}"
    ))
  }
}