package vct.col.ast.lang.c

import vct.col.ast.{CStructDeref, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.CStructDerefOps

trait CStructDerefImpl[G] extends CStructDerefOps[G] { this: CStructDeref[G] =>
  override def t: Type[G] = ???

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(struct) <> "." <> field
}