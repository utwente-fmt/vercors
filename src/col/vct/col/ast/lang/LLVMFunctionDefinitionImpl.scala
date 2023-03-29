package vct.col.ast.lang

import vct.col.ast.{Declaration, LlvmFunctionDefinition}
import vct.col.ast.util.Declarator

trait LLVMFunctionDefinitionImpl[G] extends Declarator[G] { this: LlvmFunctionDefinition[G] =>
  override def declarations: Seq[Declaration[G]] = args
}
