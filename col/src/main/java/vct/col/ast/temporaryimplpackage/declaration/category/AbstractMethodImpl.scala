package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.{AbstractMethod, Declaration, Return, Statement, Variable}
import vct.col.check.{CheckContext, CheckError}

trait AbstractMethodImpl extends ContractApplicableImpl { this: AbstractMethod =>
  override def body: Option[Statement]
  def outArgs: Seq[Variable]
  def pure: Boolean

  override def declarations: Seq[Declaration] = super.declarations ++ outArgs

  override def check(context: CheckContext): Seq[CheckError] =
    body.toSeq.flatMap(_.transSubnodes.flatMap {
      case Return(e) => e.checkSubType(returnType)
      case _ => Nil
    })
}
