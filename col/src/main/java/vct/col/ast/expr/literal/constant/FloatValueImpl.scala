package vct.col.ast.expr.literal.constant

import vct.col.ast.{FloatValue, TFloat}

trait FloatValueImpl[G] { this: FloatValue[G] =>
  assert(t.isInstanceOf[TFloat[G]])

}
