package vct.col.ast.lang

import vct.col.ast.SYCLCommandGroupDefinition
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLCommandGroupDefinitionImpl[G] { this: SYCLCommandGroupDefinition[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("command_group") <> "(" <>
      Doc.stack(Seq(
        kernel.show
      ))
      <> ")")
}