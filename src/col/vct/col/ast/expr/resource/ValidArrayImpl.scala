package vct.col.ast.expr.resource

import vct.col.ast.{TBool, Type, ValidArray}

trait ValidArrayImpl[G] { this: ValidArray[G] =>
  override def t: Type[G] = TBool()
}