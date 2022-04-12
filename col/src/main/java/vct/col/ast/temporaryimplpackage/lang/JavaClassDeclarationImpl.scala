package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.JavaClassDeclaration

trait JavaClassDeclarationImpl[G] { this: JavaClassDeclaration[G] =>
  def isStatic: Boolean
}