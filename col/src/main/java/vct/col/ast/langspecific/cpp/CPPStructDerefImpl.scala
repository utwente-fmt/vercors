package vct.col.ast.langspecific.cpp

import vct.col.ast.{CPPStructDeref, Type}

trait CPPStructDerefImpl[G] {
  this: CPPStructDeref[G] =>
  override def t: Type[G] = ???
}
