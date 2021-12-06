package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{BagMemberCount, TInt, Type}

trait BagMemberCountImpl { this: BagMemberCount =>
  override def t: Type = TInt()
}