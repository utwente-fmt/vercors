package vct.col.ast.lang

import vct.col.ast.{SilverIntToRat, TRational, Type}
import vct.col.print.{Ctx, Doc}

trait SilverIntToRatImpl[G] {
  this: SilverIntToRat[G] =>
  override def t: Type[G] = TRational()

  override def precedence: Int = perm.precedence
  override def layout(implicit ctx: Ctx): Doc = perm.show
}
