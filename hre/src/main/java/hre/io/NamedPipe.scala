package hre.io

import java.io.File

import scala.io.Source

trait NamedPipeListener {
  def onNewLine(line: String): Unit
}

object NamedPipe {
  def create(listener: NamedPipeListener, prefix: String, suffix: String): NamedPipe = {
    val os = System.getProperty("os.name")

    if(os startsWith "Windows") {
      ???
    } else {
      new UnixNamedPipe(listener, prefix, suffix)
    }
  }
}

/** A named pipe is a special type of file that has no representation on disk. Both a writing and a reading side must
  * open it before the "open" call completes. Data written to the "file" by the writer can be read from the "file" by
  * the reader. The "end of file" is reached when the writer closes the file.
  *
  * Multiple writers may open a fifo, in which case the behaviour is racy.
  *
  * This implementation does the reading side, so that the path may be given to a different process to write to. The
  * pipe is deleted after the pipe is closed by the writing side.
  */
trait NamedPipe {
  def path: String
}

class UnixNamedPipe(val listener: NamedPipeListener, prefix: String, suffix: String) extends NamedPipe {
  val path: String = {
    val file = File.createTempFile(prefix, suffix)
    val path = "/tmp/vercors-01.log" // file.getPath
    file.delete()
    new ProcessBuilder("mkfifo", path).start().waitFor()
    path
  }

  new Thread(() => {
    val f = new File(path)
    val source = Source.fromFile(f)
    source.getLines.foreach(listener.onNewLine)
    source.close()
    f.delete()
  }).start()
}
