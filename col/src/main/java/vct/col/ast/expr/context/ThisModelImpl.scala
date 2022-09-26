package vct.col.ast.expr.context

import vct.col.ast.{TModel, ThisModel, Type}

trait ThisModelImpl[G] { this: ThisModel[G] =>
  override def t: Type[G] = TModel(cls)
}