package vct.rewrite.runtime

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object CreatePrePostConditions extends RewriterBuilder {
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class CreatePrePostConditions[Pre <: Generation]() extends Rewriter[Pre] {




}