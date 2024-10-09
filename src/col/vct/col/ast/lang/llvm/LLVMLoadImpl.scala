package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMLoad, LLVMMemoryNotAtomic}
import vct.col.ast.ops.LLVMLoadOps
import vct.col.print._

trait LLVMLoadImpl[G] extends LLVMLoadOps[G] {
  this: LLVMLoad[G] =>

  private def layoutOrdering(inner: Doc)(implicit ctx: Ctx): Doc =
    if (ordering.isInstanceOf[LLVMMemoryNotAtomic[_]]) { Group(inner) }
    else { Group(inner <+> ordering) }

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text(ctx.name(variable)) <+> "=" <+>
        layoutOrdering(Text("store") <+> pointer)
    )
}
