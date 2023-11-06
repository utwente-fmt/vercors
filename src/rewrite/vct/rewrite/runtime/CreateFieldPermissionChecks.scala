package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.ast.{Class, ClassDeclaration, CodeStringClass, Declaration, InstanceField}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.rewrite.runtime.util.CodeStringDefaults

object CreateFieldPermissionChecks extends RewriterBuilder {
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class CreateFieldPermissionChecks[Pre <: Generation]() extends Rewriter[Pre] {




}