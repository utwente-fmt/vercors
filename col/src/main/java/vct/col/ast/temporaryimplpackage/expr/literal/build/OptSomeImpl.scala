package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{OptSome, TOption, Type}

trait OptSomeImpl[G] { this: OptSome[G] =>
  override def t: Type[G] = TOption(e.t)
}