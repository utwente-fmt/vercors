package vct.col.ast.expr.binder

import vct.col.ast.{ForPerm, TBool}

trait ForPermImpl[G] { this: ForPerm[G] =>
  override def t: TBool[G] = TBool()
}
