package vct.col.ast.temporaryimplpackage.expr.op.cmp

import vct.col.ast.{MapEq, TBool, Type}

trait MapEqImpl { this: MapEq =>
  override def t: Type = TBool()
}
