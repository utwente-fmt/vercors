package vct.col.ast.expr.resource

import vct.col.ast.{ContractApplicable, ResourceTerm}
import vct.col.ast.node.{NodeFamilyImpl, NodeImpl}
import vct.col.check.{CheckContext, CheckError, ResourceInPostcondition}

trait ResourceTermImpl[G] extends NodeFamilyImpl[G] {
  this: ResourceTerm[G] =>

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    super.check(context) ++
      (if (
         context.inPostCondition && context.currentApplicable.exists {
           case a: ContractApplicable[G] => a.pure
           case _ => false
         }
       ) { Seq(ResourceInPostcondition(this)) }
       else { Nil })
  }
}
