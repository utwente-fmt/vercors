package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast.{TModel, ThisModel, Type}

trait ThisModelImpl { this: ThisModel =>
  override def t: Type = TModel(cls)
}