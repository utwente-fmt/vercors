package vct.rewrite.runtime

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object RefactorGeneratedCode extends RewriterBuilder{
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class RefactorGeneratedCode[Pre<: Generation]() extends Rewriter[Pre]{

}