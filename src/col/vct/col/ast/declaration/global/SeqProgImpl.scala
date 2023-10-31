package vct.col.ast.declaration.global

import vct.col.ast.{Class, Declaration, SeqProg}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.Origin
import vct.col.print._

trait SeqProgImpl[G] extends Declarator[G] { this: SeqProg[G] =>
  override def declarations: Seq[Declaration[G]] = args ++ threads ++ decls

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")") <+> "{" <>>
        Doc.stack(threads ++ decls :+ run) <+/>
      "}"
    ))

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    super.enterCheckContext(context).withSeqProg(this)

  /*
override def check(context: CheckContext[G]): Seq[CheckError] =
    context.currentApplicable.get.body.get.transSubnodes.collectFirst {
      case label: LabelDecl[G] if label == lbl.decl => label
    } match {
      case Some(_) => Seq()
      case None => Seq(OutOfScopeError(this, lbl))
    }
  */

  /*
  - Allowed statements:
    case CommunicateX(_,_,_,_) => rewriteDefault(st)
    case VeyMontAssignExpression(_,_) => rewriteDefault (st)
    case Assign(_,_) => rewriteDefault (st)
    case Branch(_) => rewriteDefault(st)
    case Loop(_,_,_,_,_) => rewriteDefault(st)
    case Scope(_,_) => rewriteDefault(st)
    case Block(_) => rewriteDefault(st)
    case Eval(expr) => checkMethodCall(st, expr) - special requirements
    case Assert(_) => rewriteDefault(st)
   - Method calls of seqprog: Cannot have any arguments
   - method calls on endpoint: argument can only deref that endpoint
   */
  override def check(context: CheckContext[G]): Seq[CheckError] = ???

}
