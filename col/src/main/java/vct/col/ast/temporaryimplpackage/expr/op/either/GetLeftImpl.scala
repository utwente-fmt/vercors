package vct.col.ast.temporaryimplpackage.expr.op.either

import vct.col.ast.{GetLeft, Type}

trait GetLeftImpl { this: GetLeft =>
  override def t: Type = eitherType.left
}