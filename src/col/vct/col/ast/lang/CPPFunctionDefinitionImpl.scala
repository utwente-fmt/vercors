package vct.col.ast.lang

import vct.col.ast.CPPFunctionDefinition
import vct.col.print.{Ctx, Doc, Group}

trait CPPFunctionDefinitionImpl[G] { this: CPPFunctionDefinition[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Doc.spread(specs) <>> declarator
      ) <+> body.layoutAsBlock,
    ))
}