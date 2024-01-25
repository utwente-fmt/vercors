package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.rewrite.runtime.util.permissionTransfer.PermissionData
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.PermissionRewriter.permissionToRuntimeValueRewrite
import vct.rewrite.runtime.util.Util._

import scala.collection.mutable


case class RewriteContractExpr[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends AbstractQuantifierRewriter[Pre](pd) {
  override val allScopes = pd.outer.allScopes

  override def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = createAssertions(loopBodyContent.expr)

  def createAssertions(expr: Expr[Pre]): Block[Post] = {
    implicit val origin: Origin = expr.o
    val unfoldedExpr = unfoldStar(expr)
    Block[Post](unfoldedExpr.map(dispatchExpr))
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
    val permissionLocation: PermissionLocation[Pre] = FindPermissionLocation[Pre](pd).getPermission(p)(origin)
    val cond = permissionToRuntimeValueRewrite(p)
    val assertion = Assert[Post](permissionLocation.get() === cond)(null)
    Block[Post](Seq(assertion))
  }

  private def dispatchInstancePredicateApply(ipa: InstancePredicateApply[Pre]) : Block[Post] = {
    val cls: Class[Pre] = ipa.obj.t.asInstanceOf[TClass[Pre]].cls.decl
    val ip: InstancePredicate[Pre] = ipa.ref.decl
    val predicateClass: Class[Pre] = findInstancePredicateClass[Pre](cls, ip)
    val instanceMethod: InstanceMethod[Pre] = findInstancePredicateFunction[Pre](predicateClass, "getPredicate")
    val staticRef = StaticClassRef[Pre](predicateClass.ref)(predicateClass.o)
    val args: Seq[Expr[Pre]] = ipa.args :+ ipa.obj
    implicit val origin: Origin = Origin(Seq.empty)
    val mi: MethodInvocation[Pre] = MethodInvocation[Pre](
      staticRef,
      instanceMethod.ref,
      args,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Seq.empty
    )(null)
    val dispatchedMI = super.dispatch(mi)
    val newAssign = Assert[Post](dispatchedMI !== Null[Post]())(null)
    Block[Post](Seq(newAssign))
  }

}
