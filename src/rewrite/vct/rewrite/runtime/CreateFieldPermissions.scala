package vct.rewrite.runtime

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object CreateFieldPermissions extends RewriterBuilder{
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class CreateFieldPermissions[Pre<: Generation]() extends Rewriter[Pre]{

}