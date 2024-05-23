package vct.col.ast.expr.binder

import vct.col.ast.{Product, TInt, Type}
import vct.col.ast.ops.ProductOps

trait ProductImpl[G] extends ProductOps[G] { this: Product[G] =>
  override def t: Type[G] = TInt()
}
