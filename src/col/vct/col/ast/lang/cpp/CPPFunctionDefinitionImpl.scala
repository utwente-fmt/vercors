package vct.col.ast.lang.cpp

import vct.col.ast.CPPFunctionDefinition
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.CPPFunctionDefinitionOps

trait CPPFunctionDefinitionImpl[G] extends CPPFunctionDefinitionOps[G] { this: CPPFunctionDefinition[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Doc.spread(specs) <>> declarator
      ) <+> body.layoutAsBlock,
    ))
}