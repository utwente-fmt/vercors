package hre.io

import java.io.{File, FileReader, FileWriter, Reader, Writer}

case class RWFile(file: File) extends Readable with Writeable {
  override def fileName: String = file.toString
  override def isRereadable: Boolean = true

  override protected def getReader: Reader = new FileReader(file)
  override protected def getWriter: Writer = new FileWriter(file)
}
