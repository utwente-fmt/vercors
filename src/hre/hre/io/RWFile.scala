package hre.io

import java.io.{File, FileReader, FileWriter, Reader, Writer}
import java.nio.charset.StandardCharsets

case class RWFile(file: File) extends InMemoryCachedReadable with Writeable {
  override def underlyingFile: Option[File] = Some(file)
  override def fileName: String = file.toString
  override def isRereadable: Boolean = true

  override protected def getWriter: Writer = {
    invalidate()
    new FileWriter(file, StandardCharsets.UTF_8)
  }
  override protected def getReaderImpl: Reader = new FileReader(file, StandardCharsets.UTF_8)
}
