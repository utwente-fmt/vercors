package vct.col.ast.expr.model

import vct.col.ast.{ModelNew, TModel, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.ModelNewOps

trait ModelNewImpl[G] extends ModelNewOps[G] { this: ModelNew[G] =>
  override def t: Type[G] = TModel(ref)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(ref)) <> ".create()"
}