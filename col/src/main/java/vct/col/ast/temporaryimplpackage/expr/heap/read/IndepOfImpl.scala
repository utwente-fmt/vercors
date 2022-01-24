package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{IndepOf, Type}

trait IndepOfImpl[G] { this: IndepOf[G] =>
  override def t: Type[G] = e.t
}
