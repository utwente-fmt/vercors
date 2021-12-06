package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{TBool, Type, ValidMatrix}

trait ValidMatrixImpl { this: ValidMatrix =>
  override def t: Type = TBool()
}