package vct.col.ast.unsorted

import vct.col.ast.LLVMPredicateDefinition
import vct.col.ast.ops.LLVMPredicateDefinitionOps
import vct.col.print._

trait LLVMPredicateDefinitionImpl[G] extends LLVMPredicateDefinitionOps[G] {
  this: LLVMPredicateDefinition[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
