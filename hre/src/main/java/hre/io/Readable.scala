package hre.io

import java.io.Reader
import java.util.Scanner
import scala.collection.mutable

trait Readable {
  def fileName: String
  def isRereadable: Boolean
  protected def getReader: Reader

  def read[T](f: Reader => T): T = {
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
      scanner.next()
    }

  def readLines(): Seq[String] =
    read { reader =>
      val scanner = new Scanner(reader)
      // PB: Bit of a guess, but I think this is the line numbering ANTLR uses, which we want to match for origins.
      scanner.useDelimiter("\n")
      val buf = mutable.ArrayBuffer[String]()
      while(scanner.hasNext) {
        buf += scanner.next()
      }
      buf.toSeq
    }
}
