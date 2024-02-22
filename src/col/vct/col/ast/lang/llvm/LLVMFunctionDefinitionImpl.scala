package vct.col.ast.lang.llvm

import vct.col.ast.declaration.category.ApplicableImpl
import vct.col.ast.{Declaration, LLVMFunctionDefinition, Statement}
import vct.col.ast.util.Declarator
import vct.col.ast.ops.LLVMFunctionDefinitionOps

trait LLVMFunctionDefinitionImpl[G]
    extends Declarator[G]
    with ApplicableImpl[G]
    with LLVMFunctionDefinitionOps[G] {
  this: LLVMFunctionDefinition[G] =>
  override def declarations: Seq[Declaration[G]] = args

  override def body: Option[Statement[G]] = functionBody
}
