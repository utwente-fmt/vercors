package vct.col.ast.lang.llvm

import vct.col.ast._
import vct.col.ast.ops.{LLVMFunctionArgumentFamilyOps, LLVMFunctionArgumentOps}
import vct.col.print._

trait LLVMFunctionArgumentImpl[G]
    extends LLVMFunctionArgumentFamilyOps[G] with LLVMFunctionArgumentOps[G] {
  this: LLVMFunctionArgument[G] =>

  val byValType: Option[Type[G]] = {
    val bvAttrs = attributes.map {
      case LLVMByValArg(t) => Some(t)
      case _ => None
    }.filter(_.nonEmpty)
    if (bvAttrs.nonEmpty)
      bvAttrs.head
    else
      None
  }

  val isByVal: Boolean = byValType.nonEmpty

  val sretType: Option[Type[G]] = {
    attributes.filter(_.isInstanceOf[LLVMSretArg[G]]).headOption.flatMap {
      case LLVMSretArg(t) => Some(t)
    }
  }

  val isSret: Boolean = sretType.nonEmpty

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
