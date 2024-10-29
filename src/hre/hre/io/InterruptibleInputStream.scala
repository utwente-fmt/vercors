package hre.io

import java.io.InputStream

object InterruptibleInputStream {
  val BLOCK_SIZE = 1024
  val BUFFER_BLOCKS = 2
}

class InterruptibleInputStream(val underlying: InputStream)
    extends InputStream {
  outer =>
  import InterruptibleInputStream._

  private def BUFFER_SIZE = BLOCK_SIZE * BUFFER_BLOCKS
  private val buffer = new Array[Byte](BUFFER_SIZE)
  // When writeCursor and readCursor overlap, the buffer is empty. There is
  // thus one "dead" byte in the buffer.
  private var readCursor = 0
  private var writeCursor = 0
  private var eof = false

  private val transferThread = new TransferThread
  transferThread.setDaemon(true)
  transferThread.setName("[VerCors] I/O transfer thread")
  transferThread.start()

  class TransferThread extends Thread {
    override def run(): Unit =
      while (!eof) {
        if (outer.synchronized(spaceAvailable) >= BLOCK_SIZE) {
          val toRead = outer
            .synchronized(Math.min(BLOCK_SIZE, BUFFER_SIZE - writeCursor))
          val res = underlying.read(buffer, writeCursor, toRead)

          if (res == -1) { eof = true }
          else { writeCursor = (writeCursor + res) % BUFFER_SIZE }

          outer.synchronized(outer.notifyAll())
        } else { outer.synchronized(outer.wait()) }
      }
  }

  override final def read: Int =
    synchronized {
      while (true) {
        if (available > 0) {
          val result = buffer(readCursor)
          readCursor = (readCursor + 1) % BUFFER_SIZE
          notifyAll()
          return result
        } else if (eof) { return -1 }
        else { wait() }
      }

      ???
    }

  override def read(bufferOut: Array[Byte], off: Int, len: Int): Int =
    synchronized {
      var written = 0

      while (written < len) {
        if (writeCursor > readCursor) {
          val toCopy = Math.min(writeCursor - readCursor, len - written)
          System.arraycopy(buffer, readCursor, bufferOut, off + written, toCopy)
          readCursor += toCopy
          notifyAll()
          written += toCopy
        } else if (writeCursor < readCursor) {
          val toCopy = Math.min(BUFFER_SIZE - readCursor, len - written)
          System.arraycopy(buffer, readCursor, bufferOut, off + written, toCopy)
          readCursor = (readCursor + toCopy) % BUFFER_SIZE
          notifyAll()
          written += toCopy
        } else if (eof) {
          return if (written == 0)
            -1
          else
            written
        } else if (written > 0) { return written }
        else { wait() }
      }

      written
    }

  override def skip(n: Long): Long = {
    var skipped = 0L

    while (skipped < n) {
      val avail = available
      if (avail > 0 && avail < n - skipped) {
        skipped += avail
        readCursor = writeCursor
        notifyAll()
      } else if (avail > 0) {
        readCursor = (readCursor + (n - skipped).toInt) % BUFFER_SIZE
        notifyAll()
        skipped = n
      } else if (eof) { return skipped }
      else { wait() }
    }

    skipped
  }

  override def available: Int =
    if (writeCursor >= readCursor)
      writeCursor - readCursor
    else
      (writeCursor - readCursor) + BUFFER_SIZE

  def spaceAvailable: Int =
    if (readCursor > writeCursor)
      readCursor - writeCursor - 1
    else
      (readCursor - writeCursor - 1) + BUFFER_SIZE

  override def close(): Unit = underlying.close()
}
