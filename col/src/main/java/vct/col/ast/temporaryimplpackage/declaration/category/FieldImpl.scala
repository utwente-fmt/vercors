package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.{Field, Type}

trait FieldImpl[G] { this: Field[G] =>
  def t: Type[G]
}
