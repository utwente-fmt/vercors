package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapMember, TBool, Type}

trait MapMemberImpl { this: MapMember =>
  override def t: Type = TBool()
}