package vct.col.ast.expr.op.collection

import vct.col.ast.{Permutation, TBool, Type}

trait PermutationImpl[G] { this: Permutation[G] =>
  override def t: Type[G] = TBool()
}