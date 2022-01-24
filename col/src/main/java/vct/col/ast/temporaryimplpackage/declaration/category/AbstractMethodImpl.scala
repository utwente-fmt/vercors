package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.{AbstractMethod, Declaration, LabelDecl, LocalDecl, Return, Statement, Variable}
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.{Blame, CallableFailure}

trait AbstractMethodImpl[G] extends ContractApplicableImpl[G] { this: AbstractMethod[G] =>
  override def body: Option[Statement[G]]
  override def blame: Blame[CallableFailure]
  def outArgs: Seq[Variable[G]]
  def pure: Boolean

  override def declarations: Seq[Declaration[G]] = super.declarations ++ outArgs

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    super.enterCheckContext(context).withScope(transSubnodes.collect { case decl: LocalDecl[G] => decl.local }.toSet)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    body.toSeq.flatMap(_.transSubnodes.flatMap {
      case Return(e) => e.checkSubType(returnType)
      case _ => Nil
    })
}
