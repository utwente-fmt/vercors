package vct.col.ast.langspecific.cpp

import vct.col.ast.{CPPLocal, Type}

trait CPPLocalImpl[G] {
  this: CPPLocal[G] =>
  override def t: Type[G] = ???
  //todo what to do here, see the C version of this node for reference
}
