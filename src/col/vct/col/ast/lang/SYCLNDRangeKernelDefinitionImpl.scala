package vct.col.ast.lang

import vct.col.ast.SYCLNDRangeKernelDefinition
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLNDRangeKernelDefinitionImpl[G] { this: SYCLNDRangeKernelDefinition[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("command_group") <> "(" <>
      Doc.stack(Seq(
        dimensions.show,
        body.show
      ))
      <> ")")
}