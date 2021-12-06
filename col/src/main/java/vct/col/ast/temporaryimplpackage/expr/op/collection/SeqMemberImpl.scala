package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{SeqMember, TBool, Type}

trait SeqMemberImpl { this: SeqMember =>
  override def t: Type = TBool()
}