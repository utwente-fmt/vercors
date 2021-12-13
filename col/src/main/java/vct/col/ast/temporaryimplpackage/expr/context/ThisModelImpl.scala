package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast.{TModel, ThisModel, Type}

trait ThisModelImpl[G] { this: ThisModel[G] =>
  override def t: Type[G] = TModel(cls)
}