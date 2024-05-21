package hre.progress

import hre.perf.Profile
import hre.progress.task.{NameSequenceTask, SimpleNamedTask, UpdateableTask}

import java.util.{Timer, TimerTask}

case object Progress {
  val UPDATE_INTERVAL_MS: Int = 100
  val UPDATE_INTERAL_LONG_MS: Int = 700

  def install(profile: Boolean): Unit = {
    blockLayoutUpdateTimer = Some(new Timer("[VerCors] Block layout updates"))
    blockLayoutUpdate = false
    newLayoutAfterTimeout = false
    TaskRegistry.install()
    Profile.install(profile)
  }

  def finish(): Unit = synchronized {
    blockLayoutUpdateTask.foreach(_.cancel())
    blockLayoutUpdateTimer.foreach { timer =>
      timer.purge()
      timer.cancel()
    }
    blockLayoutUpdateTimer = None
    TaskRegistry.finish()
    Layout.update()
    Profile.finish()
  }

  def abort(): Unit = synchronized {
    blockLayoutUpdateTask.foreach(_.cancel())
    blockLayoutUpdateTimer.foreach { timer =>
      timer.purge()
      timer.cancel()
    }
    blockLayoutUpdateTimer = None
    TaskRegistry.abort()
    Layout.update()
    Profile.finish()
  }

  private var blockLayoutUpdateTimer: Option[Timer] = None
  private var blockLayoutUpdateTask: Option[TimerTask] = None
  private var blockLayoutUpdate = false
  private var newLayoutAfterTimeout = false

  private def delayNextUpdate(longDelay: Boolean): Unit = {
    blockLayoutUpdate = true
    blockLayoutUpdateTask.foreach(_.cancel())
    blockLayoutUpdateTimer.get.purge()
    blockLayoutUpdateTask = Some(new TimerTask {
      override def run(): Unit = Progress.synchronized {
        if (newLayoutAfterTimeout) {
          val printedLinesDidChange = Layout.update()
          newLayoutAfterTimeout = false
          delayNextUpdate(longDelay = printedLinesDidChange)
        } else {
          blockLayoutUpdate = false
        }
      }
    })
    val updateInterval = if(longDelay) UPDATE_INTERAL_LONG_MS else UPDATE_INTERVAL_MS
    blockLayoutUpdateTimer.get.schedule(blockLayoutUpdateTask.get, updateInterval)
  }

  def update(): Unit = Progress.synchronized {
    if(blockLayoutUpdate) {
      newLayoutAfterTimeout = true
    } else {
      val printedLinesDidChange = Layout.update()
      newLayoutAfterTimeout = false
      delayNextUpdate(longDelay = printedLinesDidChange)
    }
  }

  private def tryWeight(xs: IterableOnce[_]): Option[Double] =
    if(xs.knownSize > 0) Some(1.0 / xs.knownSize)
    else None

  def foreach[T](xs: IterableOnce[T], desc: T => String)(f: T => Unit): Unit =
    if(TaskRegistry.enabled) {
      val superTask = TaskRegistry.currentTaskInThread
      xs.iterator.foreach(x => {
        SimpleNamedTask(superTask, desc(x), tryWeight(xs)).frame {
          f(x)
        }
      })
    } else {
      xs.iterator.foreach(f)
    }

  def map[T, S](xs: IterableOnce[T], desc: T => String)(f: T => S): IterableOnce[S] =
    if(TaskRegistry.enabled) {
      val superTask = TaskRegistry.currentTaskInThread
      xs.iterator.map(x => {
        SimpleNamedTask(superTask, desc(x), tryWeight(xs)).frame {
          f(x)
        }
      })
    } else {
      xs.iterator.map(f)
    }

  def stages[T](names: Seq[(String, Int)])(f: (() => Unit) => T): T =
    if(TaskRegistry.enabled) {
      val sum = names.map(_._2).sum
      val weights = names.map(_._2.toDouble / sum)
      NameSequenceTask(TaskRegistry.currentTaskInThread, names.map(_._1), weights).scope(f)
    } else
      f(() => {})

  def dynamicMessages[T](count: Int)(f: (String => Unit) => T): T =
    if(TaskRegistry.enabled)
      UpdateableTask(TaskRegistry.currentTaskInThread, Some(count)).scope(f)
    else
      f(_ => {})
}