package hre.io

import java.nio.file.{FileSystem, FileSystems, Path, StandardWatchEventKinds, WatchEvent, WatchService}

/**
 * Watch facilitates that VerCors can automatically re-run when external input
 * changes. Implementors of [[Readable]] can vary how we detect external
 * changes, but typically we just listen to the change of a file just before
 * VerCors reads it.
 */
object Watch {
  private var currentWatches: Seq[Watch] = Nil

  def booleanWithWatch[T](doWatch: Boolean)(f: => T): T =
    if(doWatch) {
      currentWatches = new Watch +: currentWatches
      try {
        f
      } finally {
        currentWatches = currentWatches.tail
      }
    } else {
      f
    }

  def read(readable: Readable): Unit = {
    currentWatches.foreach { watch =>
      readable.enroll(watch)
    }
  }
}

class Watch {
  private val watchService = FileSystems.getDefault.newWatchService()

  def enroll(path: Path): Unit = {
    path.register(
      watchService,
      StandardWatchEventKinds.ENTRY_CREATE,
      StandardWatchEventKinds.ENTRY_DELETE,
      StandardWatchEventKinds.ENTRY_MODIFY,
    )

    System.console().readLine()
  }
}