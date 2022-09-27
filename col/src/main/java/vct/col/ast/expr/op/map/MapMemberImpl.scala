package vct.col.ast.expr.op.map

import vct.col.ast.{MapMember, TBool, Type}

trait MapMemberImpl[G] { this: MapMember[G] =>
  override def t: Type[G] = TBool()
}