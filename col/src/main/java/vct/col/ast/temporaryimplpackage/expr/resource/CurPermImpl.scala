package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{CurPerm, TRational, Type}

trait CurPermImpl[G] { this: CurPerm[G] =>
  override def t: Type[G] = TRational()
}