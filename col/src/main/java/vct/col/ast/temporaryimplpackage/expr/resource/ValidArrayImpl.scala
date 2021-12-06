package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{TBool, Type, ValidArray}

trait ValidArrayImpl { this: ValidArray =>
  override def t: Type = TBool()
}