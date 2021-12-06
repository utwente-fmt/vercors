package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.{Field, Type}

trait FieldImpl { this: Field =>
  def t: Type
}
