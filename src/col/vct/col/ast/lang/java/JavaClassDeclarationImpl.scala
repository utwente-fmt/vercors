package vct.col.ast.lang.java

import vct.col.ast.JavaClassDeclaration

trait JavaClassDeclarationImpl[G] { this: JavaClassDeclaration[G] =>
  def isStatic: Boolean
}