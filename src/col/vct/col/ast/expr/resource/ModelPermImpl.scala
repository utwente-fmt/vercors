package vct.col.ast.expr.resource

import vct.col.ast.{ModelPerm, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait ModelPermImpl[G] { this: ModelPerm[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("HPerm(") <> Doc.args(Seq(loc, perm)) <> ")")
}