package vct.col.ast.expr.misc

import vct.col.ast.{StringClassIntern, TStringClass, Type}

trait InternedStringImpl[G] { this: StringClassIntern[G] =>
  lazy val t: Type[G] = TStringClass()
}
