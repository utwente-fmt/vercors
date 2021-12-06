package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{OptSome, TOption, Type}

trait OptSomeImpl { this: OptSome =>
  override def t: Type = TOption(e.t)
}