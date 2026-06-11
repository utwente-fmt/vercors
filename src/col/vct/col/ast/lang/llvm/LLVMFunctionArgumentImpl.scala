package vct.col.ast.lang.llvm

import vct.col.ast._
import vct.col.ast.ops.{LLVMFunctionArgumentFamilyOps, LLVMFunctionArgumentOps}
import vct.col.print._

trait LLVMFunctionArgumentImpl[G]
    extends LLVMFunctionArgumentFamilyOps[G] with LLVMFunctionArgumentOps[G] {
  this: LLVMFunctionArgument[G] =>

  val byValType: Option[Type[G]] =
    attributes.map {
      case LLVMByValArg(t) => Some(t)
      case _ => None
    }.filter(_.nonEmpty).head

  override def layout(implicit ctx: Ctx): Doc = {
    Group(
      v.t.show <+>
        (if (attributes.nonEmpty)
           Doc.fold(attributes)((d1, d2) => d1 <+> d2)
         else
           Text("")) <+> Text(ctx.name(v))
    )
  }
}
