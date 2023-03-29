package vct.col.ast.expr.bip

import vct.col.ast.{BipLocalIncomingData, Type}

trait BipLocalIncomingDataImpl[G] { this: BipLocalIncomingData[G] =>
  override def t: Type[G] = ref.decl.t
}
