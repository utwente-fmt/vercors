package vct.col.ast.lang.llvm

import vct.col.ast.LLVMFieldDefinition
import vct.col.ast.ops.LLVMFieldDefinitionOps
import vct.col.print._
import vct.col.ast.ops.{LLVMFieldDefinitionOps, LLVMFieldDefinitionFamilyOps}

trait LLVMFieldDefinitionImpl[G]
    extends LLVMFieldDefinitionOps[G] with LLVMFieldDefinitionFamilyOps[G] {
  this: LLVMFieldDefinition[G] =>

  override def layout(implicit ctx: Ctx): Doc =
    Text(f"$offset..${offset + size}:") <+> t
}
