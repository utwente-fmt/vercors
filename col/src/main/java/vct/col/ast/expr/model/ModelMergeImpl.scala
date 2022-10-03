package vct.col.ast.expr.model

import vct.col.ast.{ModelMerge, TVoid, Type}

trait ModelMergeImpl[G] { this: ModelMerge[G] =>
  override def t: Type[G] = TVoid()
}