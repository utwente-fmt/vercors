package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteDeref
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.rewrite.runtime.util.permissionTransfer.PermissionData
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.CreatePredicates
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.LedgerHelper._
import vct.rewrite.runtime.util.PermissionRewriter.permissionToRuntimeValueRewrite
import vct.rewrite.runtime.util.Util._

import scala.collection.mutable


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
    val pt: Option[Expr[Post]] = p match {
      case Perm(AmbiguousLocation(d@Deref(t@ThisObject(_), _)), _) if d.t.isInstanceOf[PrimitiveType[Pre]]
      => ledger.miGetPermission(pd.getOffset(t), dispatch(const[Pre](findNumberPrimitiveInstanceField(program, d.ref.decl).get)))
      case Perm(AmbiguousLocation(d@Deref(t@ThisObject(_), _)), _)
      => ledger.miGetPermission(pd.getOffset(d))

      case Perm(AmbiguousLocation(d@Deref(l@Local(_), _)), _) if d.t.isInstanceOf[PrimitiveType[Pre]]
      => ledger.miGetPermission(dispatch(l), dispatch(const[Pre](findNumberPrimitiveInstanceField(program, d.ref.decl).get)))
      case Perm(AmbiguousLocation(d@Deref(l@Local(_), _)), _)
      => ledger.miGetPermission(dispatch(d))

      case Perm(AmbiguousLocation(AmbiguousSubscript(Deref(t@ThisObject(_), _), index)), _)
      => ledger.miGetPermission(pd.getOffset(t), dispatch(index))
      case Perm(AmbiguousLocation(AmbiguousSubscript(Deref(l@Local(_), _), index)), _)
      => ledger.miGetPermission(dispatch(l), dispatch(index))
      case Perm(AmbiguousLocation(AmbiguousSubscript(l@Local(_), index)), _)
      => ledger.miGetPermission(dispatch(l), dispatch(index))
      case _ => throw Unreachable(s"This type of permissions transfer is not yet supported: ${p}")
    }
    val check: Option[Expr[Post]] = pt.map(e => (e r_<=> cond) === const(1))
    val assert: Expr[Post] = check.getOrElse(tt)
    val assertion = RuntimeAssert[Post](assert, s"Permission is not enough")(null)
    Block[Post](Seq(assertion))
  }

  private def dispatchInstancePredicateApply(ipa: InstancePredicateApply[Pre]) : Block[Post] = {
    implicit val origin: Origin = ipa.o
    val ipd: InstancePredicateData[Pre] = findInstancePredicateData(ipa)
    val mi = ipd.createMethodInvocation(CreatePredicates.GETPREDICATE)
    val dispatchedMI = super.dispatch(mi)
    val newAssign = Assert[Post](dispatchedMI !== Null[Post]())(null)
    Block[Post](Seq(newAssign))
  }

}
