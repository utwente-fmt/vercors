package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{FunctionOf, Type}

trait FunctionOfImpl[G] { this: FunctionOf[G] =>
  override def t: Type[G] = binding.decl.t
}
