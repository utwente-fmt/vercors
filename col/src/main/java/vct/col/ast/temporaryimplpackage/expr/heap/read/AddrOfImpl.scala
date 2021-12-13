package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{AddrOf, TPointer, Type}

trait AddrOfImpl[G] { this: AddrOf[G] =>
  override def t: Type[G] = TPointer(e.t)
}