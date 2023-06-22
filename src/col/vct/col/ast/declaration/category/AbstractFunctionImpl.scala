package vct.col.ast.declaration.category

import vct.col.ast.{AbstractFunction, Expr}
import vct.col.check.{CheckContext, CheckMessage}
import vct.col.origin.{Blame, ContractedFailure}

trait AbstractFunctionImpl[G] extends ContractApplicableImpl[G] { this: AbstractFunction[G] =>
  override def body: Option[Expr[G]]
  def threadLocal: Boolean
  override def check(context: CheckContext[G]): Seq[CheckMessage] =
    body.toSeq.flatMap(_.checkSubType(returnType))
}
