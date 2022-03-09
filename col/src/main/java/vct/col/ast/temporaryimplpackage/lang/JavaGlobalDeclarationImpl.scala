package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaGlobalDeclaration, JavaName}
import vct.col.origin.DiagnosticOrigin

trait JavaGlobalDeclarationImpl[G] { this: JavaGlobalDeclaration[G] =>
  def pkg: Option[JavaName[G]]
  def fqn: Option[JavaName[G]]
}