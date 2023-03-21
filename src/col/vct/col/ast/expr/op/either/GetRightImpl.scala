package vct.col.ast.expr.op.either

import vct.col.ast.{GetRight, Type}

trait GetRightImpl[G] { this: GetRight[G] =>
  override def t: Type[G] = eitherType.right
}