package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{SeqMember, TBool, Type}

trait SeqMemberImpl[G] { this: SeqMember[G] =>
  override def t: Type[G] = TBool()
}