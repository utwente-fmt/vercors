package hre.io

import java.io.Reader
import java.nio.CharBuffer
import java.nio.file.{Files, Path}
import java.util.Scanner
import scala.collection.mutable

trait Readable {
  def fileName: String
  def underlyingFile: Option[Path] = None
  def isRereadable: Boolean
  protected def getReader: Reader
  def enroll(watch: Watch): Unit
  def invalidate(): Unit = {}

  def read[T](f: Reader => T): T = {
    Watch.enroll(this)
    val r = getReader
    try {
      f(r)
    } finally {
      r.close()
    }
  }

  def readToCompletion(): String =
    read { reader =>
      val scanner = new Scanner(reader)
      scanner.useDelimiter("\\A")
      if(scanner.hasNext()) scanner.next() else ""
    }

  def readLines(): Seq[String] =
    read { reader =>
      val result = mutable.ArrayBuffer[String]()

      val buffer = Array[Char](4096)
      var length = reader.read(buffer)

      var previousPosition = 0
      var position = 0

      while(length > 0) {
        val token = new StringBuilder

        while(position < length && buffer(position) != '\n')
          position += 1

        while(length > 0 && position == length) {
          token.append(CharBuffer.wrap(buffer, previousPosition, position - previousPosition))
          length = reader.read(buffer)
          previousPosition = 0
          position = 0

          while(position < length && buffer(position) != '\n')
            position += 1
        }

        token.append(CharBuffer.wrap(buffer, previousPosition, position - previousPosition))
        result += token.toString()
        position += 1
        previousPosition = position
      }
      result.toSeq
    }
}
