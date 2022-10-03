package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.rewrite.EncodeBip.IsBipComponent
import vct.col.origin.DiagnosticOrigin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object EncodeBipPermissions extends RewriterBuilder {
  override def key: String = "encodeBipPermissions"
  override def desc: String = "encodes implicit permission management of BIP models"
}

case class EncodeBipPermissions[Pre <: Generation]() extends Rewriter[Pre] {
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  override def dispatch(node: Declaration[Pre]): Unit = node match {
    case cls: Class[Pre] =>
      currentClass.having(cls) {
        rewriteDefault(cls)
      }
    case component: BipComponent[Pre] =>
      implicit val o = DiagnosticOrigin
      val fields = currentClass.top.declarations.collect { case f: InstanceField[Pre] => f }
      val diz = ThisObject(succ[Class[Post]](currentClass.top))
      val fieldPerms = AstBuildHelpers.foldStar(fields.map { f =>
        Perm(FieldLocation(diz, succ[InstanceField[Post]](f))(null), const(1))
      })
      classDeclarations.succeed(component,
        component.rewrite(invariant = Star(fieldPerms, rewriteDefault(component.invariant))(DiagnosticOrigin)))
    case x => rewriteDefault(x)
  }
}
