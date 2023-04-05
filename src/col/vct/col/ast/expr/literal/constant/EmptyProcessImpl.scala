package vct.col.ast.expr.literal.constant

import vct.col.ast.{EmptyProcess, TProcess, Type}
import vct.col.print._

trait EmptyProcessImpl[G] { this: EmptyProcess[G] =>
  override def t: Type[G] = TProcess()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("empty")
}