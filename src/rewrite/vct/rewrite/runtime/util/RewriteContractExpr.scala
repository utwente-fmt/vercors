package vct.rewrite.runtime.util

import vct.col.ast.RewriteHelpers.RewriteDeref
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.CreatePredicates
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.LedgerHelper._
import vct.rewrite.runtime.util.PermissionRewriter.permissionToRuntimeValueRewrite
import vct.rewrite.runtime.util.Util._
import vct.rewrite.runtime.util.permissionTransfer.PermissionData


case class RewriteContractExpr[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends AbstractQuantifierRewriter[Pre](pd) {
  override val allScopes = pd.outer.allScopes

  override def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = createAssertions(loopBodyContent.expr)

  val ledger: LedgerMethodBuilderHelper[Post] = pd.ledger.get


  def createAssertions(expr: Expr[Pre]): Block[Post] = {
    implicit val origin: Origin = expr.o
    val unfoldedExpr = unfoldStar(expr)
    Block[Post](unfoldedExpr.map(dispatchExpr))
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case d@Deref(t@ThisObject(_), i) => d.rewrite(obj = pd.getOffset(t))
      case _ => super.dispatch(e)
    }
  }

  private def dispatchExpr(e: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = e.o
    e match {
      case _: Star[Pre] => createAssertions(e)
      case p: Perm[Pre] => dispatchPermission(p)
      case ipa: InstancePredicateApply[Pre] => dispatchInstancePredicateApply(ipa)
      case _: Starall[Pre] | _: Exists[Pre] | _: Forall[Pre] => {
        super.dispatchQuantifier(e)
      }
      case _ => Assert[Post](super.dispatch(e))(null)
    }
  }

  private def dispatchPermission(p: Perm[Pre])(implicit origin: Origin = p.o): Block[Post] = {
    val cond = permissionToRuntimeValueRewrite(p)
    val pt: Option[Expr[Post]] = p.loc.asInstanceOf[AmbiguousLocation[Pre]].expr match {
      case d@Deref(o, _) => ledger.miGetPermission(getNewExpr(o), dispatch(const[Pre](findNumberInstanceField(program, d.ref.decl).get)))
      //      case d@Deref(_, _) => ledger.miGetPermission(getNewExpr(d))
      case AmbiguousSubscript(coll, index) => ledger.miGetPermission(getNewExpr(coll), dispatch(index))
      case _ => throw Unreachable(s"This type of permissions transfer is not yet supported: ${p}")
    }

    val check: Option[Expr[Post]] = pt.map(e => (e r_<=> cond) !== const(-1)) // test if the value is equal or bigger than the required permission
    val assert: Expr[Post] = check.getOrElse(tt)
    val assertion = RuntimeAssertExpected[Post](assert, cond, pt.getOrElse(tt), s"Permission is not enough")(null)
    Block[Post](Seq(assertion))
  }

  override def getNewExpr(e: Expr[Pre]): Expr[Post] = {
    e match {
      case d: Deref[Pre] => d.rewrite(obj = getNewExpr(d.obj))
      case t: ThisObject[Pre] => pd.getOffset(t)
      case _ => dispatch(e)
    }
  }

  private def dispatchInstancePredicateApply(ipa: InstancePredicateApply[Pre]): Block[Post] = {
    implicit val origin: Origin = ipa.o
    val allArgs: Seq[Expr[Pre]] = ipa.args :+ ipa.obj :+ StringValue(ipa.ref.decl.o.getPreferredNameOrElse())
    val dispatchedArgs: Seq[Expr[Post]] = allArgs.map(dispatch)
    val newObject = CreateObjectArray[Post](dispatchedArgs)
    val mi: MethodInvocation[Post] = ledger.miHasPredicateCheck(newObject).get
    Block[Post](Seq(Eval[Post](mi)))
  }

}
