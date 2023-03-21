package vct.col.ast.expr.op.collection

import vct.col.ast.{BagMemberCount, TInt, Type}

trait BagMemberCountImpl[G] { this: BagMemberCount[G] =>
  override def t: Type[G] = TInt()
}