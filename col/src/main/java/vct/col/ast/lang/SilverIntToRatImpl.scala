package vct.col.ast.lang

import vct.col.ast.{SilverIntToRat, TRational, Type}

trait SilverIntToRatImpl[G] { this: SilverIntToRat[G] =>
  override def t: Type[G] = TRational()
}
