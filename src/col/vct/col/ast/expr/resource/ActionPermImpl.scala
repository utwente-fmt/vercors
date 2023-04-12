package vct.col.ast.expr.resource

import vct.col.ast.{ActionPerm, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait ActionPermImpl[G] { this: ActionPerm[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("APerm(") <> Doc.args(Seq(loc, perm)) <> ")")
}
