package vct.col.ast.expr.op.collection

import vct.col.ast.{Permutation, TBool, Type}
import vct.col.ast.ops.PermutationOps

trait PermutationImpl[G] extends PermutationOps[G] { this: Permutation[G] =>
  override def t: Type[G] = TBool()
}