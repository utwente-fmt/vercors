package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{AddrOf, TPointer, Type}

trait AddrOfImpl { this: AddrOf =>
  override def t: Type = TPointer(e.t)
}