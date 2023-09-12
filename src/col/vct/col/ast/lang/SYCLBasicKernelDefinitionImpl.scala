package vct.col.ast.lang

import vct.col.ast.SYCLBasicKernelDefinition
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLBasicKernelDefinitionImpl[G] { this: SYCLBasicKernelDefinition[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("command_group") <> "(" <>
      Doc.stack(Seq(
        dimensions.show,
        body.show
      ))
      <> ")")
}