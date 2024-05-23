package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast.{PrimitiveType, _}
import vct.col.util.AstBuildHelpers._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.LedgerHelper.{LedgerMethodBuilderHelper, LedgerRewriter}
import vct.rewrite.runtime.util.Util.isExtendingThread
import vct.rewrite.runtime.util.permissionTransfer.PermissionData
import vct.rewrite.runtime.util.{RewriteContractExpr, TransferPermissionRewriter}

object AddPermissionsOnCreate extends RewriterBuilder {
  override def key: String = "addPermissionsOnCreate"

  override def desc: String = "Creates the permissions inside the constructor for each InstanceField of the object"
}


case class AddPermissionsOnCreate[Pre <: Generation]() extends Rewriter[Pre] {

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
    val test = program.rewrite(declarations = newDecl)
    test
  }

  /**
   * Dispatches declarations
   * The java constructor is dispatched and so that permissions are initialized for each instance field
   * the class is dispatched to keep track of the current class
   * @param decl
   */
  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case jc: JavaConstructor[Pre] => dispatchJC(jc)
      case cls: Class[Pre] => currentClass.having(cls) {
        super.dispatch(cls)
      }
      case _ => super.dispatch(decl)
    }
  }

  /**
   * Dispatches the java constructor so that it initializes the permissions for each instance field
   * if the thread extends the thread class also a join token is set
   * @param jc
   * @return
   */
  private def dispatchJC(jc: JavaConstructor[Pre]): JavaConstructor[Post] = {
    implicit val origin: Origin = jc.o
    val instanceFields = currentClass.top.declarations.collect { case i: InstanceField[Pre] => i }
    val size = instanceFields.size
    val obj = ThisObject[Post](this.anySucc(currentClass.top))
    val initInstanceFieldsExpr: MethodInvocation[Post] = if (size == 0) ledger.miInitiatePermission(obj).get else ledger.miInitiatePermission(obj, const(size)).get
    val initInstanceFieldsPerm = Eval[Post](initInstanceFieldsExpr)
    val newBody = Block[Post](Seq(dispatch(jc.body), initInstanceFieldsPerm, createJoinTokenCall))
    classDeclarations.succeed(jc, jc.rewrite(body = newBody))
  }

  /**
   * Creates a join token for the thread if the class extends the thread class
   * @return
   */
  def createJoinTokenCall : Block[Post] = {
    implicit val origin: Origin = DiagnosticOrigin
    if (!isExtendingThread(currentClass.top)) {
      return Block(Nil)
    }
    val mi = ledger.miSetJoinToken(ThisObject[Post](this.anySucc(currentClass.top)), RuntimeFractionOne[Post]()).get
    Block[Post](Seq(Eval[Post](mi)))
  }

  /**
   * When a new array is created the permissions are initialized for the array
   * @param stat
   * @return
   */
  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case a@Assign(location, na@NewArray(_, dims, _, _)) =>
        val dispatchedAssign = super.dispatch(a)
        val initPermissionArray = Eval[Post](ledger.miInitiatePermission(dispatch(location), dispatch(dims.head)).get)(DiagnosticOrigin)
        Block[Post](Seq(dispatchedAssign, initPermissionArray))(DiagnosticOrigin)
      case a@Eval(PreAssignExpression(location, na@NewArray(_, dims, _, _))) =>
        val dispatchedAssign = super.dispatch(a)
        val initPermissionArray = Eval[Post](ledger.miInitiatePermission(dispatch(location), dispatch(dims.head)).get)(DiagnosticOrigin)
        Block[Post](Seq(dispatchedAssign, initPermissionArray))(DiagnosticOrigin)
      case a@Eval(PostAssignExpression(location, na@NewArray(_, dims, _, _))) =>
        val dispatchedAssign = super.dispatch(a)
        val initPermissionArray = Eval[Post](ledger.miInitiatePermission(dispatch(location), dispatch(dims.head)).get)(DiagnosticOrigin)
        Block[Post](Seq(dispatchedAssign, initPermissionArray))(DiagnosticOrigin)
      case _ => super.dispatch(stat)
    }
  }
}