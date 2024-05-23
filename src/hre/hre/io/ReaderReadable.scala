package hre.io
import java.io.Reader

/**
 * Constructs a readable from a [[java.io.Reader]]. If this reader is for a file, use [[RWFile]] instead: otherwise
 * it will mess up the watch file tracking.
 */
case class ReaderReadable(fileName: String, reader: Reader) extends Readable {
  override def isRereadable: Boolean = false

  override protected def getReader: Reader = reader

  override def enroll(watch: Watch): Unit = {
    // Reader of unknown origin, so we can't do anything.
  }
}
