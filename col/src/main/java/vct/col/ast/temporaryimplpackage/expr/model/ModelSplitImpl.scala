package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelSplit, TVoid, Type}

trait ModelSplitImpl { this: ModelSplit =>
  override def t: Type = TVoid()
}