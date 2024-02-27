package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.runtime.util.LedgerHelper.{LedgerMethodBuilderHelper, LedgerRewriter}
import vct.rewrite.runtime.util.Util.findClosestInjectivityMap
import vct.rewrite.runtime.util.{RewriteContractExpr, TransferPermissionRewriter}
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

object CreateLocking extends RewriterBuilder {
  override def key: String = "createLocking"

  override def desc: String = "Creates locking for the constructor and synchronized key word"
}


case class CreateLocking[Pre <: Generation]() extends Rewriter[Pre] {

  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()

  implicit var program: Program[Pre] = _
  implicit var ledger: LedgerMethodBuilderHelper[Post] = _

  /**
   * Dispatch of the program for debugging and using the program everywhere to look up specific instancefields
   *
   * @param program Program node
   * @return The rewritten program
   */
  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    lazy val newDecl: Seq[GlobalDeclaration[Post]] = globalDeclarations.collect {
      val (ledgerHelper, _, otherDeclarations) = LedgerRewriter[Pre](this).rewriteLedger(program)
      ledger = ledgerHelper
      otherDeclarations.foreach(dispatch)
    }._1
    program.rewrite(declarations = newDecl)
  }

  /**
   * Transform the constructor to remove the permissions of the lock invariant
   * @param decl
   */
  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case jc: JavaConstructor[Pre] => dispatchJC(jc)
      case cls: Class[Pre] => currentClass.having(cls) {super.dispatch(cls)}
      case _ => super.dispatch(decl)
    }
  }

  /**
   * Change the constructor to remove the permissions of the lock invariant
   * @param jc
   */
  private def dispatchJC(jc: JavaConstructor[Pre]) : Unit = {
    implicit val origin: Origin = jc.o
    val pd: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top).setLedger(ledger)
    val removePermissions: Block[Post] = TransferPermissionRewriter[Pre](pd).removePermissions(currentClass.top.intrinsicLockInvariant)
    val newBody = Block[Post](Seq(dispatch(jc.body), removePermissions))
    classDeclarations.succeed(jc, jc.rewrite(body = newBody))
  }

  /**
   * Dispatch the synchronized statement to add and remove the permissions of the lock invariant inside the synchronized statement
   * @param stat
   * @return
   */
  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    stat match {
      case s: Synchronized[Pre] => dispatchSynchronized(s)
      case _ => super.dispatch(stat)
    }
  }

  /**
   * Transforms the synchronized block to transfer the permissions of the lock invariant
   * At the end of the block the permissions are removed and the lock invariant is checked
   * @param s
   * @return
   */
  private def dispatchSynchronized(s: Synchronized[Pre]): Synchronized[Post] = {
    implicit val origin: Origin = s.o
    val injectivityMap = findClosestInjectivityMap(variables.freeze)
    val lockInvariant: Expr[Pre] = s.obj.t.asInstanceOf[TClass[Pre]].cls.decl.intrinsicLockInvariant
    val pd: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top).setLedger(ledger).setInjectivityMap(injectivityMap).setOffset(dispatch(s.obj))
    val transferrer = TransferPermissionRewriter[Pre](pd)
    val addPermissions: Block[Post] = transferrer.addPermissions(lockInvariant)
    val check: Statement[Post] = RewriteContractExpr[Pre](pd).createAssertions(lockInvariant)
    val removePermissions: Block[Post] = transferrer.removePermissions(lockInvariant)
    val newBody = Block[Post](Seq(addPermissions, dispatch(s.body), check, removePermissions))
    s.rewrite(body = newBody)
  }

  /**
   * Dispatch committed expression to be changed to true
   * @param lc
   * @return
   */
  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case c: Committed[Pre] => tt[Post]
      case _ => super.dispatch(e)
    }
  }
}