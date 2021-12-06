package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{TBoundedInt, Type, WritePerm}

trait WritePermImpl { this: WritePerm =>
  override def t: Type = TBoundedInt(1, 2)
}