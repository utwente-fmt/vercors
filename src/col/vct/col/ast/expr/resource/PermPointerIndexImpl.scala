package vct.col.ast.expr.resource

import vct.col.ast.{PermPointerIndex, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait PermPointerIndexImpl[G] {
  this: PermPointerIndex[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\pointer_index(") <> Doc.args(Seq(p, idx, perm)) <> ")")
}
