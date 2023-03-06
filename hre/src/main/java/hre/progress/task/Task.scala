package hre.progress.task

import hre.perf.ResourceUsage
import hre.progress.TaskRegistry

import scala.collection.mutable.ArrayBuffer

abstract class Task {
  private val subTasks = ArrayBuffer[Task]()

  protected var startUsage: ResourceUsage = null
  private var usageReported: ResourceUsage = ResourceUsage(0, 0, 0, 0, 0, 0)

  def superTask: Task

  def profilingBreadcrumb: String
  def profilingTrail: Seq[String] = profilingBreadcrumb +: superTask.profilingTrail

  def progressText: String
  def progress: Double = 0.5

  def poll(): ResourceUsage = this.synchronized {
    val usage = TaskRegistry.ownUsage()
    val delta = usage - startUsage - usageReported
    TaskRegistry.reportUsage(delta, profilingTrail)
    usageReported += delta
    usage
  }

  def start(): Unit = superTask.synchronized {
    startUsage = superTask.poll()
    superTask.subTasks += this
    TaskRegistry.mostRecentlyStartedTaskInThread.set(this)
  }

  def end(): Unit = superTask.synchronized {
    poll()
    superTask.subTasks -= this
    superTask.usageReported += usageReported
  }

  private def prefix(tail: String, prefix: String, maxWidth: Int): String = {
    val sep = " › "
    val ellipsis = "…"

    if(prefix.length + sep.length + tail.length <= maxWidth)
      prefix + sep + tail
    else if(4 + ellipsis.length + sep.length + tail.length <= maxWidth) {
      val room = maxWidth - (ellipsis.length + sep.length + tail.length)
      val leftChars = room / 2
      prefix.substring(0, leftChars) +
        ellipsis +
        prefix.substring(prefix.length - room + leftChars) +
        sep +
        tail
    } else if(ellipsis.length + sep.length + tail.length <= maxWidth) {
      ellipsis + sep + tail
    } else tail
  }

  def progressLines(maxWidth: Int): Seq[String] = {
    val subLines = subTasks.toIndexedSeq.map(_.progressLines(maxWidth))
    val myText = progressText.split('\n')

    subLines match {
      case Nil => myText
      case Seq(lines) =>
        if(myText.length == 1) lines.init :+ prefix(lines.last, myText.head, maxWidth)
        else lines ++ Seq("-".repeat(maxWidth)) ++ myText
      case multipleSubtasks =>
        multipleSubtasks.flatten ++ Seq("-".repeat(maxWidth)) ++ myText
    }
  }

  def frame[T](f: => T): T =
    try {
      start()
      f
    } finally {
      end()
    }

  def frame1[I, O](f: I => O): I => O =
    (i) => try {
      start()
      f(i)
    } finally {
      end()
    }
}