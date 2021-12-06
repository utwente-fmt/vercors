package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelChoose, TVoid, Type}

trait ModelChooseImpl { this: ModelChoose =>
  override def t: Type = TVoid()
}