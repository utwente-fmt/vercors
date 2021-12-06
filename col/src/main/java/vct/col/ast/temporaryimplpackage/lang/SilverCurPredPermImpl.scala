package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{SilverCurPredPerm, TRational, Type}

trait SilverCurPredPermImpl { this: SilverCurPredPerm =>
  override def t: Type = TRational()
}