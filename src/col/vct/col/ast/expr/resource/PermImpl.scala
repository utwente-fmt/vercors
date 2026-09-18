package vct.col.ast.expr.resource

import vct.col.ast.expr.binder.PossibleTriggerImpl
import vct.col.ast.{Perm, TResource, Type, PredicateLocation}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.PermOps

trait PermImpl[G] extends PermOps[G] with PossibleTriggerImpl[G] {
  this: Perm[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("Perm(") <> Doc.args(Seq(loc, perm)) <> ")")

  override def isPossibleTrigger: Boolean =
    loc match {
      case t: PredicateLocation[G] => true
      case _ => false
    }
}
