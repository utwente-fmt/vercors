package vct.col.ast.lang

import vct.col.ast.CPPNamespaceDefinition
import vct.col.print._

trait CPPNamespaceDefinitionImpl[G] {
  this: CPPNamespaceDefinition[G] =>

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Text("namespace") <+> Text(name) <+> "{" <>> Doc.stack(declarations) <+/>
        "}"
    ))
}
