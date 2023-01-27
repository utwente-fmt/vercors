package vct.col.ast.expr.context

import vct.col.ast.{ThisStringClass, TStringClass, Type}

trait ThisStringClassImpl[G] { this: ThisStringClass[G] =>
  lazy val t: Type[G] = TStringClass()
}
