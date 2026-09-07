package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMAllocAOps
import vct.col.ast.LLVMAllocA
import vct.col.print._

trait LLVMAllocAImpl[G] extends LLVMAllocAOps[G] {
  this: LLVMAllocA[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text(ctx.name(variable)) <+> "=" <+> Group(
        Text("alloca") <+> returnType.asPointer.map(_.element.show)
          .getOrElse(Text("INVALID")) <> "," <+> numElements
      )
    )
}
