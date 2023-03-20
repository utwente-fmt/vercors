package hre.progress

import hre.perf.Profile
import hre.progress.task.{NameSequenceTask, SimpleNamedTask, UpdateableTask}

import java.util.{Timer, TimerTask}

case object Progress {
  val UPDATE_INTERVAL_MS: Int = 100

  def install(forceProgress: Boolean, profile: Boolean): Unit = {
    TaskRegistry.install()
    Layout.install(forceProgress)
    Profile.install(profile)
  }

  def finish(): Unit = {
    blockLayoutUpdateTask.foreach(_.cancel())
    blockLayoutUpdateTimer.purge()
    TaskRegistry.finish()
    Profile.finish()
  }

  def abort(): Unit = {
    blockLayoutUpdateTask.foreach(_.cancel())
    blockLayoutUpdateTimer.purge()
    TaskRegistry.abort()
    Profile.finish()
  }

  private val blockLayoutUpdateTimer = new Timer()
  private var blockLayoutUpdateTask: Option[TimerTask] = None
  private var blockLayoutUpdate = false
  private var newLayoutAfterTimeout = false

  private val noProgressTimer = new Timer()
  private var noProgressTask: Option[TimerTask] = None

  private def delayNextUpdate(): Unit = {
    blockLayoutUpdate = true
    blockLayoutUpdateTask.foreach(_.cancel())
    blockLayoutUpdateTimer.purge()
    blockLayoutUpdateTask = Some(new TimerTask {
      override def run(): Unit = Progress.synchronized {
        if (newLayoutAfterTimeout) {
          Layout.update()
          newLayoutAfterTimeout = false
          delayNextUpdate()
        } else {
          blockLayoutUpdate = false
        }
      }
    })
    blockLayoutUpdateTimer.schedule(blockLayoutUpdateTask.get, UPDATE_INTERVAL_MS)
  }

  def update(): Unit = Progress.synchronized {
    if(blockLayoutUpdate) {
      newLayoutAfterTimeout = true
    } else {
      Layout.update()
      newLayoutAfterTimeout = false
      delayNextUpdate()
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