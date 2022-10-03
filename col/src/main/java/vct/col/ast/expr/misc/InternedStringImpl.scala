package vct.col.ast.expr.misc

import vct.col.ast.{InternedString, Type}

trait InternedStringImpl[G] { this: InternedString[G] =>
  override def t: Type[G] = interner.decl.returnType
}
