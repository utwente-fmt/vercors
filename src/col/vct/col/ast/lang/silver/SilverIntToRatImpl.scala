package vct.col.ast.lang.silver

import vct.col.ast.{SilverIntToRat, TRational, Type}
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.SilverIntToRatOps

trait SilverIntToRatImpl[G] extends SilverIntToRatOps[G] { this: SilverIntToRat[G] =>
  override def t: Type[G] = TRational()

  override def precedence: Int = perm.precedence
  override def layout(implicit ctx: Ctx): Doc = perm.show
}
