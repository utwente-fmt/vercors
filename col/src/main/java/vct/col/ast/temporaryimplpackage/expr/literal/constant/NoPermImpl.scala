package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{NoPerm, TBoundedInt, Type}

trait NoPermImpl { this: NoPerm =>
  override def t: Type = TBoundedInt(0, 1)
}