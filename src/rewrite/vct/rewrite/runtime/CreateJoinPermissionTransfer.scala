package vct.rewrite.runtime

import vct.col.ast.Program
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

object CreateJoinPermissionTransfer extends RewriterBuilder{
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class CreateJoinPermissionTransfer[Pre<: Generation]() extends Rewriter[Pre]{

  /*

    Idea for implementing of the joining permission.
    The annotator writes:

    resource postJoin(frac field1, frac field2, frac field3, frac field4)

   */

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }
}