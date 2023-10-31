package vct.col.ast.statement

import vct.col.ast.{Assert, Assign, Block, Branch, Communicate, CommunicateX, Eval, Loop, ParAssign, Scope, Statement, VeyMontAssignExpression}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.check.{CheckContext, CheckError, SeqProgStatement}
import vct.col.print._

trait StatementImpl[G] extends NodeFamilyImpl[G] { this: Statement[G] =>
  def layoutAsBlock(implicit ctx: Ctx): Doc =
    Text("{") <>> foldBlock(_ <+/> _) <+/> "}"

  def foldBlock(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc = show

  override def check(context: CheckContext[G]): Seq[CheckError] = context.currentSeqProg match {
    case None => Seq()
    case Some(_) => this match {
      case
        _: CommunicateX[G] |
        _: Communicate[G] |
        _: VeyMontAssignExpression[G] |
        _: Assign[G] |
        _: ParAssign[G] |
        _: Branch[G] |
        _: Loop[G] |
        _: Scope[G] |
        _: Block[G] |
        _: Eval[G] |
        _: Assert[G] => Seq()
      case _ => Seq(SeqProgStatement(this))
    }
  }
}
