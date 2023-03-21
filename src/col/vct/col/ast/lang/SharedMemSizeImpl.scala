package vct.col.ast.lang

import vct.col.ast.{SharedMemSize, TInt, Type}

trait SharedMemSizeImpl[G] { this: SharedMemSize[G] =>
  override def t: Type[G] = TInt()
}