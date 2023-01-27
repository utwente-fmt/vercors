package vct.col.ast.lang

import vct.col.ast.{JavaName, JavaNamespace}
import vct.col.origin.DiagnosticOrigin
import vct.col.resolve.lang.Java

trait JavaNamespaceImpl[G] { this: JavaNamespace[G] =>
  def isJavaLang(): Boolean = pkg.map(_.names).contains(Java.JAVA_LANG)
}