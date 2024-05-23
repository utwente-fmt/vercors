package vct.col.ast.lang.c

import vct.col.ast.CFunctionDefinition
import vct.col.ast.ops.CFunctionDefinitionOps
import vct.col.print.{Ctx, Doc, Group}

trait CFunctionDefinitionImpl[G] extends CFunctionDefinitionOps[G] {
  this: CFunctionDefinition[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Doc.spread(specs) <>> declarator) <+> body.layoutAsBlock,
    ))
}
