package vct.col.ast.expr.model

import vct.col.ast.{ModelDestroy, TVoid, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait ModelDestroyImpl[G] { this: ModelDestroy[G] =>
  override def t: Type[G] = TVoid()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(model) <> "." <> "destroy" <> "(" <> ")"
}