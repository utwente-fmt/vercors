package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast.temporaryimplpackage.node.NodeFamilyImpl
import vct.col.ast.{Result, Type}
import vct.col.check.{CheckContext, CheckError, ResultOutsidePostcondition}

trait ResultImpl[G] extends NodeFamilyImpl[G] { this: Result[G] =>
  override def t: Type[G] = applicable.decl.returnType

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if(context.inPostCondition) Nil else Seq(ResultOutsidePostcondition(this))
}