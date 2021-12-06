package vct.col.ast.temporaryimplpackage.expr.binder

import vct.col.ast.{Exists, TBool, Type}

trait ExistsImpl { this: Exists =>
  override def t: Type = TBool()
}
