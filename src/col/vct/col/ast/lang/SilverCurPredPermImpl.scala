package vct.col.ast.lang

import vct.col.ast.{SilverCurPredPerm, TRational, Type}

trait SilverCurPredPermImpl[G] { this: SilverCurPredPerm[G] =>
  override def t: Type[G] = TRational()
}