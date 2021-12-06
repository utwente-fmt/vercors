package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.{AbstractFunction, Expr}
import vct.col.check.{CheckContext, CheckError}

trait AbstractFunctionImpl extends ContractApplicableImpl { this: AbstractFunction =>
  override def body: Option[Expr]
  override def check(context: CheckContext): Seq[CheckError] =
    body.toSeq.flatMap(_.checkSubType(returnType))
}
