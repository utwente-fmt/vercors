package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{SetMember, TBool, Type}

trait SetMemberImpl { this: SetMember =>
  override def t: Type = TBool()
}