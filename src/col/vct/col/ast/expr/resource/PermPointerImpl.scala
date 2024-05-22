package vct.col.ast.expr.resource

import vct.col.ast.{PermPointer, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.PermPointerOps

trait PermPointerImpl[G] extends PermPointerOps[G] { this: PermPointer[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\pointer(") <> Doc.args(Seq(p, len, perm)) <> ")")
}