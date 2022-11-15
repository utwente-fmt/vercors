package vct.col.ast.lang

import vct.col.ast.{JavaName, JavaNamespace}
import vct.col.origin.DiagnosticOrigin

trait JavaNamespaceImpl[G] { this: JavaNamespace[G] =>
  lazy val name: Option[String] = pkg match {
    case Some(value) => Some(value.names.mkString("."))
    case None => None
  }
}