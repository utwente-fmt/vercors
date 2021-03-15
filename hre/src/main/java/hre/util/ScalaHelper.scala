package hre.util

import scala.annotation.varargs

object ScalaHelper {
  @varargs
  def toIterable(fields: String*): Iterable[String] = Seq("args")
}
