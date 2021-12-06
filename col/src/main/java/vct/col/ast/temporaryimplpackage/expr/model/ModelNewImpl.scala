package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelNew, TModel, Type}

trait ModelNewImpl { this: ModelNew =>
  override def t: Type = TModel(ref)
}