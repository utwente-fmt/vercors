package vct.col.rewrite.bip

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.bip.EncodeBipPermissions.{GeneratedBipFieldPermissionOrigin, GeneratedBipPermissionsOrigin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers._

case object EncodeBipPermissions extends RewriterBuilder {
  override def key: String = "encodeBipPermissions"
  override def desc: String = "encodes implicit permission management of BIP models"

  private def GeneratedBipPermissionsOrigin(component: BipComponent[_]): Origin = {
    component.o
  }

  private def GeneratedBipFieldPermissionOrigin(component: BipComponent[_], field: InstanceField[_]): Origin = {
    field.o.replacePrefName(s"bipPerm${field.o.getPreferredName.get.preferredName.capitalize}")
  }
}

case class EncodeBipPermissions[Pre <: Generation]() extends Rewriter[Pre] {
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  override def dispatch(node: Declaration[Pre]): Unit = node match {
    case cls: Class[Pre] =>
      currentClass.having(cls) {
        rewriteDefault(cls)
      }
    case component: BipComponent[Pre] =>
      implicit val o = GeneratedBipPermissionsOrigin(component)
      val fields = currentClass.top.declarations.collect { case f: InstanceField[Pre] if !f.isFinal => f }
      val diz = ThisObject(succ[Class[Post]](currentClass.top))
      val fieldPerms = AstBuildHelpers.foldStar(fields.map { f =>
        implicit val o = GeneratedBipFieldPermissionOrigin(component, f)
        Perm(FieldLocation(diz, succ[InstanceField[Post]](f)), const(1))
      })
      classDeclarations.succeed(component,
        component.rewrite(invariant = Star(fieldPerms, rewriteDefault(component.invariant))))
    case x => rewriteDefault(x)
  }
}
