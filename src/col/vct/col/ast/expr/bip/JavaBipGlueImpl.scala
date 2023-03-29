package vct.col.ast.expr.bip

import vct.col.ast.{JavaBipGlue, TNotAValue, Type}

trait JavaBipGlueImpl[G] { this: JavaBipGlue[G] =>
  override def t: Type[G] = new TNotAValue()
}
