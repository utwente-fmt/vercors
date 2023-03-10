package hre.progress.task

import hre.perf.ResourceUsage
import hre.progress.{Progress, TaskRegistry}

import scala.collection.mutable.ArrayBuffer

abstract class AbstractTask {
  private val subTasks = ArrayBuffer[AbstractTask]()

  protected var startUsage: Option[ResourceUsage] = None
  private var usageReported: ResourceUsage = ResourceUsage.zero

  def progressWeight: Option[Double] = None
  protected var progressDone: Double = 0.0

  def progress: Double = {
    val subtaksProgress = subTasks.map(t => t.progressWeight -> t.progress)
    val knownWeightProgress = subtaksProgress.collect { case Some(weight) -> progress => weight * progress }.sum[Double]
    val knownDone = progressDone + knownWeightProgress
    val unknownWeightProgress = subtaksProgress.collect { case None -> progress => progress }.sortBy(-_)
    val result = unknownWeightProgress.foldLeft(knownDone) {
      case (progress, subtaskProgress) => progress + ((1.0 - progress) * 0.1 * subtaskProgress)
    }

    if(result > 1.0) {
      println(s"Discarding ${result - 1.0} from $result at $profilingBreadcrumb")
      1.0
    } else result
  }

  private var ownerThread = -1L
  def getOwnerThread: Long = ownerThread

  def superTaskOrRoot: Option[AbstractTask]

  def profilingBreadcrumb: String
  def profilingTrail: Seq[String] = profilingBreadcrumb +: superTaskOrRoot.toSeq.flatMap(_.profilingTrail)

  def progressText: String

  def poll(): ResourceUsage = this.synchronized {
    if(Thread.currentThread().getId != ownerThread)
      return TaskRegistry.ownUsage()

    val usage = TaskRegistry.ownUsage()
    val delta = usage - startUsage.get - usageReported
    TaskRegistry.reportUsage(delta, profilingTrail)
    usageReported += delta
    usage
  }

  def start(): Unit = {
    ownerThread = Thread.currentThread().getId

    superTaskOrRoot match {
      case None =>
        startUsage = Some(TaskRegistry.ownUsage())
        TaskRegistry.reportUsage(startUsage.get, Seq("<untracked>"))
      case Some(superTask) =>
        superTask.synchronized {
          startUsage = Some(superTask.poll())
          superTask.subTasks += this
        }
    }

    TaskRegistry.push(this)
    Progress.update()
  }

  def end(): Unit = {
    assert(subTasks.isEmpty)
    poll()

    superTaskOrRoot match {
      case None =>
      case Some(superTask) =>
        superTask.synchronized {
          superTask.subTasks -= this
          superTask.usageReported += usageReported
          superTask.progressDone += progressWeight.getOrElse(0.1 * (1.0 - superTask.progressDone))
        }
    }

    usageReported = ResourceUsage.zero
    startUsage = None
    ownerThread = -1L

    TaskRegistry.pop(this)
    Progress.update()
  }

  def abort(): Unit = {
    subTasks.toIndexedSeq.foreach(_.abort())
    end()
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
    val myText = progressText.split('\n').map(_.take(maxWidth))

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