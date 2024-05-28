package vct.col.ast.expr.binder

import vct.col.ast.{Sum, TInt, Type}
import vct.col.ast.ops.SumOps

trait SumImpl[G] extends SumOps[G] {
  this: Sum[G] =>
  override def t: Type[G] = TInt()
}
