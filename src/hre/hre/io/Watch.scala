package hre.io

import com.typesafe.scalalogging.LazyLogging
import sun.misc.{Signal, SignalHandler}

import java.nio.file.{ClosedWatchServiceException, FileSystem, FileSystems, Path, StandardWatchEventKinds, WatchEvent, WatchService}

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
  private var fileWatchService = FileSystems.getDefault.newWatchService()

  private var triggered: Boolean = false

  private var threads: Seq[Thread] = Nil

  private var interrupts = 0

  def enroll(path: Path): Unit = {
    path.getParent.register(
      fileWatchService,
      StandardWatchEventKinds.ENTRY_CREATE,
      StandardWatchEventKinds.ENTRY_DELETE,
      StandardWatchEventKinds.ENTRY_MODIFY,
    )
  }

  private def fileWatchThread() = new Thread {
    override def run(): Unit = try {
      fileWatchService.take()
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

  private def addThread(t: Thread): Unit = {
    threads = t +: threads
    t.setDaemon(true)
    t.start()
  }

  private def signal(): Unit = synchronized {
    triggered = true
    notifyAll()
  }

  private def reset(): Unit = {
    fileWatchService.close()
    fileWatchService = FileSystems.getDefault.newWatchService()

    for(thread <- threads) {
      thread.interrupt()
    }

    for(thread <- threads) {
      thread.join()
    }

    threads = Nil

    triggered = false

    interrupts = 0
  }

  def await(): Unit = synchronized {
    addThread(fileWatchThread())
    addThread(stdinWatchThread())

    logger.info(s"[Waiting for input files to change - press ENTER to run again manually]")

    while(!triggered) {
      try {
        wait()
      } catch {
        case _: InterruptedException =>
          if(interrupts == 0) {
            logger.info(s"[Press Ctrl+C again to exit VerCors]")
            interrupts = 1
          } else {
            throw new InterruptedException()
          }
      }
    }

    reset()
  }
}