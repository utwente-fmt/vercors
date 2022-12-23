package hre.io

import java.io.Writer

trait Writeable {
  def fileName: String
  protected def getWriter: Writer

  def write[T](f: Writer => T): T = {
    val writer = getWriter
    try {
      f(writer)
    } finally {
      writer.close()
    }
  }
}
