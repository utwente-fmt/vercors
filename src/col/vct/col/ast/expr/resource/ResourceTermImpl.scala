package vct.col.ast.expr.resource

import vct.col.ast.{ContractApplicable, ResourceTerm}
import vct.col.ast.node.NodeImpl
import vct.col.check.{CheckContext, CheckError, ResourceInPostcondition}

trait ResourceTermImpl[G] extends NodeImpl[G] {
  this: ResourceTerm[G] =>

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if (
      context.inPreCondition &&
      context.currentApplicable.exists { case a: ContractApplicable[G] =>
        a.pure
      }
    ) { Seq(ResourceInPostcondition(this)) }
    else { Nil }
}
