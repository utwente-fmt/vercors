package vct.col.ast.lang

import vct.col.ast.CStructDeclaration
import vct.col.print.{Ctx, Doc, Text, Empty}

trait CStructDeclarationImpl[G] { this: CStructDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Doc.stack(Seq(
      Text("struct") <+>
      (if (name.isEmpty) Empty else Text(name.get)) <+>
        "{" <>> Doc.stack(decl) <+/> "}"
    ))
  }
}