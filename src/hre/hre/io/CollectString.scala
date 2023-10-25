package hre.io

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

object CollectString {
  def apply(f: PrintStream => Unit): String = {
    val bytes = new ByteArrayOutputStream
    val print = new PrintStream(bytes)
    f(print)
    new String(bytes.toByteArray, StandardCharsets.UTF_8)
  }
}
