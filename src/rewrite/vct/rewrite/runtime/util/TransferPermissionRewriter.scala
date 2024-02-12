package vct.rewrite.runtime.util

import vct.col.ast.RewriteHelpers.RewriteDeref
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.CreatePredicates
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.LedgerHelper._
import vct.rewrite.runtime.util.PermissionRewriter._
import vct.rewrite.runtime.util.Util.{InstancePredicateData, findInstancePredicateClass, findInstancePredicateData, findInstancePredicateFunction}
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

case class TransferPermissionRewriter[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends AbstractQuantifierRewriter[Pre](pd) {
  override val allScopes = pd.outer.allScopes

  implicit var add: Boolean = _

  val ledger: LedgerMethodBuilderHelper[Post] = pd.ledger.get

  def addPermissions(predicate: Expr[Pre]): Block[Post] = {
    add = true
    transferPermissions(predicate)
  }

  def removePermissions(predicate: Expr[Pre]): Block[Post] = {
    add = false
    transferPermissions(predicate)
  }

  override def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = {
    if (add) {
      TransferPermissionRewriter(pd).addPermissions(loopBodyContent.expr)
    } else {
      TransferPermissionRewriter(pd).removePermissions(loopBodyContent.expr)
    }
  }


  def dispatchExpr(e: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = e.o
    e match {
      case p: Perm[Pre] => Eval[Post](dispatchPerm(p))
      case ipa: InstancePredicateApply[Pre] if add => dispatchInstancePredicateApplyAdd(ipa)
      case ipa: InstancePredicateApply[Pre] if !add => dispatchInstancePredicateApplyRemove(ipa)
      case s: Starall[Pre] => super.dispatchQuantifier(s) //Let the AbstractQuantifier rewrite the StarAll, since it is the only one that can hold permissions
      case _ => Block[Post](Seq.empty)
    }
  }

  private def op(a: Expr[Post], b: Expr[Post])(implicit origin: Origin): Expr[Post] = if (add) a r_+ b else a r_- b

  private def unfoldPredicate(predicate: Expr[Pre]): Seq[Expr[Pre]] =
    unfoldStar(predicate).collect { case p@(_: Perm[Pre] | _: Starall[Pre] | _: InstancePredicateApply[Pre]) => p }

  private def transferPermissions(predicate: Expr[Pre]): Block[Post] = {
    implicit val origin: Origin = predicate.o
    Block[Post](unfoldPredicate(predicate).map(dispatchExpr))
  }

  private def dispatchPerm(p: Perm[Pre])(implicit origin: Origin): Expr[Post] = {
    val newValue: Expr[Post] = pd.factored(permissionToRuntimeValueRewrite(p))

    val pt: Option[Expr[Post]] = p.loc.asInstanceOf[AmbiguousLocation[Pre]].expr match {
      case d@Deref(o, _) => {
        val getPerm = ledger.miGetPermission(getNewExpr(o), locationExpression(d.ref.decl)).get
        ledger.miSetPermission(getNewExpr(o), locationExpression(d.ref.decl), op(getPerm, newValue))
      }
//      case d@Deref(_, _) => {
//        val getPerm = ledger.miGetPermission(getNewExpr(d)).get
//        ledger.miSetPermission(getNewExpr(d), op(getPerm, newValue))
//      }
      case AmbiguousSubscript(coll, index) => {
        val getPerm = ledger.miGetPermission(getNewExpr(coll), dispatch(index)).get
        ledger.miSetPermission(getNewExpr(coll), dispatch(index), op(getPerm, newValue))
      }
      case _ => throw Unreachable(s"This type of permissions transfer is not yet supported: ${p}")
    }
    pt.getOrElse(tt)
  }

  override def getNewExpr(e: Expr[Pre]): Expr[Post] = {
    e match {
      case d: Deref[Pre] => d.rewrite(obj = getNewExpr(d.obj))
      case t: ThisObject[Pre] => pd.getOffset(t)
      case _ => dispatch(e)
    }
  }

  private def locationExpression(instanceField: InstanceField[Pre])(implicit origin: Origin): Expr[Post] = {
    dispatch(const[Pre](findNumberInstanceField(program, instanceField).get))
  }



  private def dispatchInstancePredicateApplyAdd(ipa: InstancePredicateApply[Pre]) : Block[Post] = {
    implicit val origin: Origin = ipa.o
    val ipd: InstancePredicateData[Pre] = findInstancePredicateData(ipa)
    val addPermissions: Block[Post] = TransferPermissionRewriter(pd).addPermissions(ipa.ref.decl.body.get)
    val foldPredicateMI = ipd.createMethodInvocation(CreatePredicates.FOLD)
    val evalMI = Eval[Post](super.dispatch(foldPredicateMI))
    Block[Post](Seq(addPermissions, evalMI))
  }

  private def dispatchInstancePredicateApplyRemove(ipa: InstancePredicateApply[Pre]) : Block[Post] = {
    implicit val origin: Origin = ipa.o
    val ipd: InstancePredicateData[Pre] = findInstancePredicateData(ipa)
    val foldPredicateMI = ipd.createMethodInvocation(CreatePredicates.UNFOLD)
    val evalMI = Eval[Post](super.dispatch(foldPredicateMI))
    val removePermissions: Block[Post] = TransferPermissionRewriter(pd).removePermissions(ipa.ref.decl.body.get)
    Block[Post](Seq(removePermissions, evalMI))
  }
}