package vct.col.ast.expr.misc

import vct.col.ast.{Intern, TStringClass, Type}

trait InternedStringImpl[G] { this: Intern[G] =>
  lazy val t: Type[G] = TStringClass()
}
