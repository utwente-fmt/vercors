package vct.col.ast.lang

import vct.col.ast.CFunctionDefinition
import vct.col.print.{Ctx, Doc, Group, Text}

trait CFunctionDefinitionImpl[G] { this: CFunctionDefinition[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Doc.spread(specs) <>> declarator
      ) <+> body.layoutAsBlock,
    ))
}