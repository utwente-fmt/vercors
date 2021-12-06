package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{CurPerm, TRational, Type}

trait CurPermImpl { this: CurPerm =>
  override def t: Type = TRational()
}