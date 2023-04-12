package vct.col.ast.lang

import vct.col.ast.{CStructDeref, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait CStructDerefImpl[G] { this: CStructDeref[G] =>
  override def t: Type[G] = ???

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(struct) <> "." <> field
}