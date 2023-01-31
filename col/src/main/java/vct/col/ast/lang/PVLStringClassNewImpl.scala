package vct.col.ast.lang

import vct.col.ast.{PVLStringClassNew, TStringClass, Type}

trait PVLStringClassNewImpl[G] { this: PVLStringClassNew[G] =>
  def t: Type[G] = TStringClass()
}
