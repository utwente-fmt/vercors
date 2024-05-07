package hre.io

import com.typesafe.scalalogging.LazyLogging
import sun.misc.{Signal, SignalHandler}

import java.nio.file.{ClosedWatchServiceException, FileSystem, FileSystems, Path, StandardWatchEventKinds, WatchEvent, WatchKey, WatchService}
import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.control.Breaks.{break, breakable}

/**
 * Watch facilitates that VerCors can automatically re-run when external input
 * changes. Implementors of [[Readable]] can vary how we detect external
 * changes, but typically we just listen to the change of a file just before
 * VerCors reads it.
 */
object Watch {
  private var currentWatch: Option[Watch] = None

  def booleanWithWatch[T](doWatch: Boolean, default: T)(f: => T): T =
    if(doWatch) {
      val watch = new Watch(Thread.currentThread())
      currentWatch = Some(watch)
      val sigint = new Signal("INT")
      val previousHandler = Signal.handle(sigint, _ => Watch.currentWatch.get.mainThread.interrupt())

      var result: T = default

      try {
        while(true) {
          try {
            result = f
          } catch {
            case _: InterruptedException =>
              // f was interrupted - wait until something changes.
          }
          watch.await()
        }

        ???
      } catch {
        case _: InterruptedException =>
          // the watch loop was interrupted - finish up.
          result
      } finally {
        Signal.handle(sigint, previousHandler)
        currentWatch = None
      }
    } else {
      f
    }

  def enroll(readable: Readable): Unit = {
    currentWatch.foreach { watch => readable.enroll(watch) }
  }
}

class Watch(val mainThread: Thread) extends LazyLogging { watch =>
  private val watchServices = mutable.Map[FileSystem, WatchService]()
  private val watchedDirs = mutable.Set[Path]()
  private val watchedFiles = mutable.Set[Path]()

  private var toInvalidate: Seq[Readable] = Nil

  private var triggered: Boolean = false

  private var threads: Seq[Thread] = Nil

  private var debounceInterrupt = true

  def watchCount: Int =
    watchedFiles.size

  def enroll(dirtyPath: Path): Unit = {
    val path = dirtyPath.toAbsolutePath.normalize()
    val parent = Option(path.getParent).getOrElse(path)
    if(!watchedDirs.contains(parent)) {
      watchedDirs += parent
      val fs = parent.getFileSystem
      val ws = watchServices.getOrElseUpdate(fs, fs.newWatchService())
      parent.register(
        ws,
        StandardWatchEventKinds.ENTRY_CREATE,
        StandardWatchEventKinds.ENTRY_DELETE,
        StandardWatchEventKinds.ENTRY_MODIFY,
      )
    }

    watchedFiles += path
  }

  def invalidate(readable: Readable): Unit = {
    toInvalidate = readable +: toInvalidate
  }

  private def isWatchedFile(key: WatchKey): Boolean = {
    val parent = key.watchable().asInstanceOf[Path]
    val events = key.pollEvents().asScala.toSeq
    logger.warn(s"Events: ${events.map(_.context()).mkString(", ")}")
    key.reset()
    for(event <- events) {
      val relPath = event.context().asInstanceOf[Path]

      if(watchedFiles.contains(parent.resolve(relPath)) || watchedFiles.contains(parent))
        return true
    }

    false
  }

  private def fileWatchThread() = new Thread {
    override def run(): Unit = try {
      watchServices.values.toSeq match {
        case Nil => return
        case Seq(ws) =>
          // Typical: normally all plain files belong to one file system.
          while(!isWatchedFile(ws.take())) {}
        case watchServices =>
          breakable {
            while (true) {
              // We spend approximately 0.1s per pass over all [[WatchService]]s
              for (ws <- watchServices) {
                val key = Option(ws.poll(100_000 / watchServices.size, TimeUnit.MICROSECONDS))

                key match {
                  case Some(key) if isWatchedFile(key) => break()
                  case _ => // continue
                }
              }
            }
          }
      }
      watch.signal()
    } catch {
      case _: InterruptedException => // do nothing
      case _: ClosedWatchServiceException => // do nothing
    }
  }

  private def stdinWatchThread() = new Thread {
    override def run(): Unit = try {
      var res = System.in.read()

      while(res != -1 && res != '\n'.toInt) {
        res = System.in.read()
      }

      if(res == '\n'.toInt) {
        watch.signal()
      }
    } catch {
      case _: InterruptedException => // do nothing
    }
  }

  private def disableDebounceThread() = new Thread {
    override def run(): Unit = try {
      Thread.sleep(1000)
      debounceInterrupt = false
    } catch {
      case _: InterruptedException => // do nothing
    }
  }

  private def addThread(t: Thread): Unit = {
    threads = t +: threads
    t.setName("[VerCors] Watch loop")
    t.setDaemon(true)
    t.start()
  }

  private def signal(): Unit = synchronized {
    triggered = true
    notifyAll()
  }

  private def reset(): Unit = {
    for(ws <- watchServices.values) {
      ws.close()
    }

    watchServices.clear()
    watchedDirs.clear()
    watchedFiles.clear()

    for(inv <- toInvalidate) {
      inv.invalidate()
    }

    toInvalidate = Nil

    for(thread <- threads) {
      thread.interrupt()
    }

    for(thread <- threads) {
      thread.join()
    }

    threads = Nil

    triggered = false

    debounceInterrupt = true
  }

  def await(): Unit = synchronized {
    addThread(fileWatchThread())
    addThread(stdinWatchThread())

    addThread(disableDebounceThread())

    logger.info(s"[Waiting for ${watchedFiles.mkString("[", ", ", "]")} to change - press ENTER to run again manually]")

    while(!triggered) {
      try {
        wait()
      } catch {
        case _: InterruptedException =>
          if(debounceInterrupt) {
            logger.info(s"[Press Ctrl+C again to exit VerCors]")
            debounceInterrupt = false
          } else {
            throw new InterruptedException()
          }
      }
    }

    reset()
  }
}