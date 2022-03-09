package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaName, JavaNamespace}
import vct.col.origin.DiagnosticOrigin

trait JavaNamespaceImpl[G] { this: JavaNamespace[G] =>
  def fqn: Option[JavaName[G]] = pkg
}