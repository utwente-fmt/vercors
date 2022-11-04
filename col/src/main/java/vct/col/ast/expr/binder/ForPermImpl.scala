package vct.col.ast.expr.binder

import vct.col.ast.{ForPerm, TResource}

trait ForPermImpl[G] { this: ForPerm[G] =>
  override def t: TResource[G] = TResource()
}
