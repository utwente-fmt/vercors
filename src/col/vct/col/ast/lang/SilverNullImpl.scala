package vct.col.ast.lang

import vct.col.ast.{SilverNull, TRef}

trait SilverNullImpl[G] { this: SilverNull[G] =>
  override def t: TRef[G] = TRef[G]()
}
