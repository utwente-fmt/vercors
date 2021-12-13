package vct.col.ast.temporaryimplpackage.expr.op.either

import vct.col.ast.{GetLeft, Type}

trait GetLeftImpl[G] { this: GetLeft[G] =>
  override def t: Type[G] = eitherType.left
}