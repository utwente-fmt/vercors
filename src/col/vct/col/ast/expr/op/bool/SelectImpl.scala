package vct.col.ast.expr.op.bool

import vct.col.ast.{Select, Type}
import vct.col.typerules.Types

trait SelectImpl[G] { this: Select[G] =>
  override lazy val t: Type[G] =
    Types.leastCommonSuperType(whenTrue.t, whenFalse.t)
}