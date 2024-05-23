package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewObject, TClass, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.NewObjectOps

trait NewObjectImpl[G] extends NewObjectOps[G] {
  this: NewObject[G] =>
  override def t: Type[G] = TClass(cls, Seq())

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> ctx.name(cls) <> "()"
}
