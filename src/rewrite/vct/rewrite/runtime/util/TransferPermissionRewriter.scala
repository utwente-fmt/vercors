package vct.rewrite.runtime.util

import vct.col.ast.RewriteHelpers.RewriteDeref
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.LedgerHelper._
import vct.rewrite.runtime.util.Util._
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

case class TransferPermissionRewriter[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends AbstractQuantifierRewriter[Pre](pd) {
  override val allScopes = pd.outer.allScopes

  implicit var add: Boolean = _

  val ledger: LedgerMethodBuilderHelper[Post] = pd.ledger.get

  /**
   * Adds the permissions to the thread
   * @param predicate
   * @return
   */
  def addPermissions(predicate: Expr[Pre]): Block[Post] = {
    add = true
    transferPermissions(predicate)
  }

  /**
   * Removes the permissions from the thread
   * @param predicate
   * @return
   */
  def removePermissions(predicate: Expr[Pre]): Block[Post] = {
    add = false
    transferPermissions(predicate)
  }

  /**
   * Dispatches the loop body content
   * @param loopBodyContent
   * @param origin
   * @return the block returns by the addPermissions or removePermissions
   */
  override def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = {
    if (add) {
      TransferPermissionRewriter(pd).addPermissions(loopBodyContent.expr)
    } else {
      TransferPermissionRewriter(pd).removePermissions(loopBodyContent.expr)
    }
  }

  /**
   * Dispatches the expression and only permissions and starall are dispatched to a block, the rest is dispatched to a empty block
   * @param e
   * @param origin
   * @return the transferring statement block
   */
  def dispatchExpr(e: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = e.o
    e match {
      case p: Perm[Pre] => Eval[Post](dispatchPerm(p))
      case s: Starall[Pre] => super.dispatchQuantifier(s) //Let the AbstractQuantifier rewrite the StarAll, since it is the only one that can hold permissions
      case _ => Block[Post](Seq.empty)
    }
  }

  /**
   * If the class needs to add use the + operator, otherwise use the - operator
   * @param a
   * @param b
   * @param origin
   * @return the function of the operator
   */
  private def op(a: Expr[Post], b: Expr[Post])(implicit origin: Origin): Expr[Post] = if (add) a r_+ b else a r_- b

  /**
   * Unfolds the predicate expression and collects the permissions
   * @param predicate
   * @return the permissions
   */
  private def unfoldPredicate(predicate: Expr[Pre]): Seq[Expr[Pre]] =
    unfoldStar(predicate).collect { case p@(_: Perm[Pre] | _: Starall[Pre] | _: InstancePredicateApply[Pre]) => p }

  /**
   * transforms the predicate expression to a block of statements that add/remove permissions of a thread
   * @param predicate
   * @return
   */
  private def transferPermissions(predicate: Expr[Pre]): Block[Post] = {
    implicit val origin: Origin = predicate.o
    Block[Post](unfoldPredicate(predicate).map(dispatchExpr))
  }

  /**
   * Dispatches the permission and adds/removes the permission to the thread for normal permissions/ array permissions
   * @param p
   * @param origin
   * @return the transferm permission statement
   */
  private def dispatchPerm(p: Perm[Pre])(implicit origin: Origin): Expr[Post] = {
    val newValue: Expr[Post] = pd.factored(permissionToRuntimeValueRewrite(p))

    val pt: Option[Expr[Post]] = p.loc.asInstanceOf[AmbiguousLocation[Pre]].expr match {
      case d@Deref(o, _) => {
        val newDataObject: MethodInvocation[Post] = ledger.pmbh.miCreate(
          CreateObjectArray[Post](
            Seq(getNewExpr(o),locationExpression(d.ref.decl))
          )).get

        val getPerm = ledger.miGetPermission(newDataObject).get
        ledger.miSetPermission(newDataObject, op(getPerm, newValue))
      }
      case AmbiguousSubscript(coll, index)  => {
        val newDataObject: MethodInvocation[Post] = ledger.pmbh.miCreate(
          CreateObjectArray[Post](Seq(getNewExpr(coll), dispatch(index)))).get
        val getPerm = ledger.miGetPermission(newDataObject).get
        ledger.miSetPermission(newDataObject, op(getPerm, newValue))
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

}