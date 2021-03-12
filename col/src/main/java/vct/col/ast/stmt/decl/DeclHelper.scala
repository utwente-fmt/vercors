package vct.col.ast.stmt.decl

import scala.annotation.varargs

object DeclHelper {
  @varargs
  def toIterable(fields: String*): Iterable[String] = Seq("args")
}
