package hre.io

import java.io.{Reader, Writer}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

case class RWFile(file: Path, doWatch: Boolean = true) extends InMemoryCachedReadable with Writeable {
  override def underlyingFile: Option[Path] = Some(file)
  override def fileName: String = file.toString
  override def isRereadable: Boolean = true

  override protected def getWriter: Writer = {
    invalidate()
    Files.newBufferedWriter(file, StandardCharsets.UTF_8)
  }

  override protected def getReaderImpl: Reader = {
    Files.newBufferedReader(file, StandardCharsets.UTF_8)
  }

  override def enroll(watch: Watch): Unit = {
    if(doWatch) watch.enroll(file)
    watch.invalidate(this)
  }
}
