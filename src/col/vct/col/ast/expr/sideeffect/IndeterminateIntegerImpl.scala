package vct.col.ast.expr.sideeffect

import vct.col.ast.{IndeterminateInteger, TInt, Type}
import vct.col.ast.ops.IndeterminateIntegerOps

trait IndeterminateIntegerImpl[G] extends IndeterminateIntegerOps[G] { this: IndeterminateInteger[G] =>
  override def t: Type[G] = TInt()
}
