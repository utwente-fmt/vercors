package vct.col.ast.lang.llvm

import vct.col.ast.declaration.category.ApplicableImpl
import vct.col.ast.{Declaration, LlvmFunctionDefinition, Statement}
import vct.col.ast.util.Declarator
import vct.col.ast.ops.LlvmFunctionDefinitionOps

trait LlvmFunctionDefinitionImpl[G] extends Declarator[G] with ApplicableImpl[G] with LlvmFunctionDefinitionOps[G] { this: LlvmFunctionDefinition[G] =>
  override def declarations: Seq[Declaration[G]] = args

  override def body: Option[Statement[G]] = Some(functionBody)
}
