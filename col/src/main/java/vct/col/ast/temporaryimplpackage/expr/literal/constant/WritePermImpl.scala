package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{TBoundedInt, Type, WritePerm}

trait WritePermImpl[G] { this: WritePerm[G] =>
  override def t: Type[G] = TBoundedInt(1, 2)
}