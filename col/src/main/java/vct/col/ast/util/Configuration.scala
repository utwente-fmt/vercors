package vct.col.ast.util

import vct.col.ast.syntax.{JavaDialect, JavaSyntax, Syntax}

object Configuration {
  /**
   * Get the syntax that is to be used for diagnostic output.
   */
  def getDiagSyntax: Syntax = JavaSyntax.getJava(JavaDialect.JavaVerCors)
}
