package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{Select, Type}
import vct.col.util.Types

trait SelectImpl { this: Select =>
  override def t: Type =
    Types.leastCommonSuperType(whenTrue.t, whenFalse.t)
}