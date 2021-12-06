package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelMerge, TVoid, Type}

trait ModelMergeImpl { this: ModelMerge =>
  override def t: Type = TVoid()
}