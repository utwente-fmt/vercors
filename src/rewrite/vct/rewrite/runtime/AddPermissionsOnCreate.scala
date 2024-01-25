package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.permissionTransfer.PermissionData
import vct.rewrite.runtime.util.{RewriteContractExpr, TransferPermissionRewriter}

object AddPermissionsOnCreate extends RewriterBuilder {
  override def key: String = "addPermissionsOnCreate"

  override def desc: String = "Creates the permissions inside the constructor for each InstanceField of the object"
}


case class AddPermissionsOnCreate[Pre <: Generation]() extends Rewriter[Pre] {

  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()

  implicit var program: Program[Pre] = _

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
      case cls: Class[Pre] => currentClass.having(cls) {super.dispatch(cls)}
      case _ => super.dispatch(decl)
    }
  }

  private def dispatchJC(jc: JavaConstructor[Pre]) = {
    implicit val origin: Origin = jc.o
    val instanceFields = currentClass.top.declarations.collect{case i: InstanceField[Pre] => i}
    val obj = ThisObject[Pre](currentClass.top.ref)
    val permissions : Expr[Pre] = instanceFields
      .map(i => Deref[Pre](obj, i.ref)(null))
      .map(d => AmbiguousLocation(d)(null))
      .map(a => Perm[Pre](a, const(1)))
      .foldLeft[Expr[Pre]](tt[Pre])((acc, elem) => acc &* elem)

    val pd: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top)
    val addPermissions: Block[Post] = TransferPermissionRewriter[Pre](pd).addPermissions(permissions)
    val newBody = Block[Post](Seq(addPermissions, dispatch(jc.body)))
    classDeclarations.succeed(jc, jc.rewrite(body = newBody))
  }
}