package vct.col.ast.expr.misc

import vct.col.ast.{StringClassConcat, TStringClass, Type}

trait StringClassConcatImpl[G] { this: StringClassConcat[G] =>
  lazy val t: Type[G] = TStringClass()
}
