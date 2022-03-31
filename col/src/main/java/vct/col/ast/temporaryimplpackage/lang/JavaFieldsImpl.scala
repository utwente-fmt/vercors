package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaFields, JavaStatic}

trait JavaFieldsImpl[G] { this: JavaFields[G] =>
  override def isStatic = modifiers.contains(JavaStatic[G]())
}