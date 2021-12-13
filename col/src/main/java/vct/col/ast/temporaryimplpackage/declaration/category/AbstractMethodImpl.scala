package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.{AbstractMethod, Declaration, Return, Statement, Variable}
import vct.col.check.{CheckContext, CheckError}

trait AbstractMethodImpl[G] extends ContractApplicableImpl[G] { this: AbstractMethod[G] =>
  override def body: Option[Statement[G]]
  def outArgs: Seq[Variable[G]]
  def pure: Boolean

  override def declarations: Seq[Declaration[G]] = super.declarations ++ outArgs

  override def check(context: CheckContext[G]): Seq[CheckError] =
    body.toSeq.flatMap(_.transSubnodes.flatMap {
      case Return(e) => e.checkSubType(returnType)
      case _ => Nil
    })
}
