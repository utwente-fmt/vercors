package vct.col.ast.expr.op.bool

import vct.col.ast.{Select, Type}
import vct.col.util.Types

trait SelectImpl[G] { this: Select[G] =>
  override def t: Type[G] =
    Types.leastCommonSuperType(whenTrue.t, whenFalse.t)
}