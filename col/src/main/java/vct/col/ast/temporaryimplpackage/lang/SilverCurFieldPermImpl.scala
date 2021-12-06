package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{SilverCurFieldPerm, TRational, Type}

trait SilverCurFieldPermImpl { this: SilverCurFieldPerm =>
  override def t: Type = TRational()
}