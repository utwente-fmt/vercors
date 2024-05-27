package vct.col.ast.expr.heap.read

import vct.col.ast.{FunctionOf, Type}
import vct.col.print._
import vct.col.ast.ops.FunctionOfOps

trait FunctionOfImpl[G] extends FunctionOfOps[G] {
  this: FunctionOf[G] =>
  override def t: Type[G] = binding.decl.t

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> ctx.name(binding) <> "!" <>
      Doc.fold(vars.map(ctx.name(_)).map(Text))(_ <> "," <+> _) <> ")"
}
