package vct.col.ast.temporaryimplpackage.expr.op.either

import vct.col.ast.{GetRight, Type}

trait GetRightImpl { this: GetRight =>
  override def t: Type = eitherType.right
}