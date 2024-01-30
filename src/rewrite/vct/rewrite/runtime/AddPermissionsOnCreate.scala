package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast.{PrimitiveType, _}
import vct.col.util.AstBuildHelpers._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.LedgerHelper.{LedgerMethodBuilderHelper, LedgerRewriter}
import vct.rewrite.runtime.util.permissionTransfer.PermissionData
import vct.rewrite.runtime.util.{RewriteContractExpr, TransferPermissionRewriter}

object AddPermissionsOnCreate extends RewriterBuilder {
  override def key: String = "addPermissionsOnCreate"

  override def desc: String = "Creates the permissions inside the constructor for each InstanceField of the object"
}


case class AddPermissionsOnCreate[Pre <: Generation]() extends Rewriter[Pre] {

  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()

  implicit var program: Program[Pre] = _

  def ledger: LedgerMethodBuilderHelper[Pre] = LedgerMethodBuilderHelper[Pre](program)


  /**
   * Dispatch of the program for debugging and using the program everywhere to look up specific instancefields
   *
   * @param program Program node
   * @return The rewritten program
   */
  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case jc: JavaConstructor[Pre] => dispatchJC(jc)
      case cls: Class[Pre] => currentClass.having(cls) {
        super.dispatch(cls)
      }
      case _ => super.dispatch(decl)
    }
  }

  private def dispatchJC(jc: JavaConstructor[Pre]): JavaConstructor[Post] = {
    implicit val origin: Origin = jc.o
    val instanceFields = currentClass.top.declarations.collect {
      case i: InstanceField[Pre] if i.t.isInstanceOf[PrimitiveType[Pre]] => i
    }
    val size = instanceFields.size
    val obj = ThisObject[Pre](currentClass.top.ref)
    val initInstanceFieldsExpr: MethodInvocation[Pre] = if(size == 0) ledger.miInitiatePermission(obj).get else ledger.miInitiatePermission(obj, const(size)).get
    val initInstanceFieldsPerm = Eval[Post](dispatch(initInstanceFieldsExpr))
    val newBody = Block[Post](Seq(dispatch(jc.body), initInstanceFieldsPerm))
    classDeclarations.succeed(jc, jc.rewrite(body = newBody))
  }
}