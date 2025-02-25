package vct.col.ast.expr.literal.constant

import vct.col.ast.{ReadPerm, TFraction, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.ReadPermOps

trait ReadPermImpl[G] extends ReadPermOps[G] {
  this: ReadPerm[G] =>
  override def t: Type[G] = TFraction()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => Text("wildcard")
      case _ => Text("read")
    }
}
