package vct.col.ast.expr.literal.constant

import vct.col.ast.{TVoid, Type, Void}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait VoidImpl[G] { this: Void[G] =>
  override def t: Type[G] = TVoid()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("void")
}