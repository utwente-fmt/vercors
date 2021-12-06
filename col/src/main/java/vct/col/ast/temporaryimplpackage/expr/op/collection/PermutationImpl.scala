package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Permutation, TBool, Type}

trait PermutationImpl { this: Permutation =>
  override def t: Type = TBool()
}