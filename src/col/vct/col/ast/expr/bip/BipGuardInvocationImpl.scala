package vct.col.ast.expr.bip

import vct.col.ast.{BipGuardInvocation, TBool, Type}

trait BipGuardInvocationImpl[G] { this: BipGuardInvocation[G] =>
  override def t: Type[G] = TBool()
}
