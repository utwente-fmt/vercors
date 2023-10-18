package hre.io

import java.io.{Reader, StringReader}
import java.util.Scanner

trait InMemoryCachedReadable extends Readable {
  protected def getReaderImpl: Reader

  protected var cache: Option[String] = None

  private def ensureCache(): Unit =
    if (cache.isEmpty) {
      val scanner = new Scanner(getReaderImpl)
      scanner.useDelimiter("\\A")
      cache = Some(
        if (scanner.hasNext())
          scanner.next()
        else
          ""
      )
    }

  override protected def getReader: Reader = {
    ensureCache()
    new StringReader(cache.get)
  }

  override def readToCompletion(): String = {
    ensureCache()
    cache.get
  }

  protected var linesCache: Option[Seq[String]] = None

  override def readLines(): Seq[String] = {
    if (linesCache.isEmpty) { linesCache = Some(super.readLines()) }
    linesCache.get
  }

  protected def invalidate(): Unit = {
    cache = None
    linesCache = None
  }
}
