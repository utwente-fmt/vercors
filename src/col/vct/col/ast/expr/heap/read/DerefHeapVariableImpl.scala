package vct.col.ast.expr.heap.read

import vct.col.ast.{DerefHeapVariable, Type}
import vct.col.print._

trait DerefHeapVariableImpl[G] {
  this: DerefHeapVariable[G] =>
  override def t: Type[G] = ref.decl.t

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))
}
