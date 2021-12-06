package vct.col.ast.temporaryimplpackage.expr.binder

import vct.col.ast.{Sum, TInt, Type}

trait SumImpl { this: Sum =>
  override def t: Type = TInt()
}
