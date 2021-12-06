package vct.col.ast.temporaryimplpackage.expr.op.cmp

import vct.col.ast.{MapDisjoint, TBool, Type}

trait MapDisjointImpl { this: MapDisjoint =>
  override def t: Type = TBool()
}
