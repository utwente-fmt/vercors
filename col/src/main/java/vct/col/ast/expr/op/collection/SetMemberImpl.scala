package vct.col.ast.expr.op.collection

import vct.col.ast.{SetMember, TBool, Type}

trait SetMemberImpl[G] { this: SetMember[G] =>
  override def t: Type[G] = TBool()
}