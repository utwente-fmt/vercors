package vct.col.ast.expr.literal.constant

import vct.col.ast.{NoPerm, TBoundedInt, Type}

trait NoPermImpl[G] { this: NoPerm[G] =>
  override def t: Type[G] = TBoundedInt(0, 1)
}