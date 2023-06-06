package vct.col.ast.lang

import vct.col.ast.declaration.category.ApplicableImpl
import vct.col.ast.{Declaration, LlvmFunctionDefinition, Statement}
import vct.col.ast.util.Declarator

trait LLVMFunctionDefinitionImpl[G] extends Declarator[G] with ApplicableImpl[G] { this: LlvmFunctionDefinition[G] =>
  override def declarations: Seq[Declaration[G]] = args

  override def body: Option[Statement[G]] = Some(functionBody)
}
