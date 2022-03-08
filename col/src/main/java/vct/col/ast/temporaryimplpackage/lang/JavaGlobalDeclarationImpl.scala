package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaGlobalDeclaration, JavaName}

trait JavaGlobalDeclarationImpl[G] { this: JavaGlobalDeclaration[G] =>
  def pkg: Option[JavaName[G]]
}