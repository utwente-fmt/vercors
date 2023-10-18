package hre.progress.task

import hre.perf.ResourceUsage
import hre.progress.{Progress, ProgressRender, TaskRegistry}

import scala.collection.mutable.ArrayBuffer

abstract class AbstractTask {
  private val subTasks = ArrayBuffer[AbstractTask]()

  protected var startUsage: Option[ResourceUsage] = None
  private var usageReported: ResourceUsage = ResourceUsage.zero

  def progressWeight: Option[Double] = None
  protected var progressDone: Double = 0.0

  def progress: Double =
    synchronized {
      val subtaksProgress = subTasks.map(t => t.progressWeight -> t.progress)
      val knownWeightProgress = subtaksProgress.collect {
        case Some(weight) -> progress => weight * progress
      }.sum[Double]
      val knownDone = progressDone + knownWeightProgress
      val unknownWeightProgress = subtaksProgress.collect {
        case None -> progress => progress
      }.sortBy(-_)
      val result =
        unknownWeightProgress.foldLeft(knownDone) {
          case (progress, subtaskProgress) =>
            progress + ((1.0 - progress) * 0.1 * subtaskProgress)
        }

      if (result > 1.0) {
        println(
          s"Discarding ${result - 1.0} from $result at $profilingBreadcrumb"
        )
        1.0
      } else
        result
    }

  private var ownerThread = -1L
  def getOwnerThread: Long = ownerThread

  def superTaskOrRoot: Option[AbstractTask]

  def profilingBreadcrumb: String
  def profilingTrail: Seq[String] =
    profilingBreadcrumb +: superTaskOrRoot.toSeq.flatMap(_.profilingTrail)

  def renderHere: ProgressRender
  def renderHereShort: ProgressRender = renderHere

  def poll(): ResourceUsage =
    synchronized {
      if (Thread.currentThread().getId != ownerThread)
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
          superTask.progressDone +=
            progressWeight.getOrElse(0.1 * (1.0 - superTask.progressDone))
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

  private def renderProgressWith(depthForDetail: Int): ProgressRender =
    synchronized {
      val here =
        if (depthForDetail <= 0)
          renderHere
        else
          renderHereShort
      val sub = subTasks.toIndexedSeq

      sub match {
        case Nil => here
        case Seq(sub) =>
          val subRender = sub.renderProgressWith(depthForDetail - 1)
          if (subRender.lines.size == 1)
            here.postfix(ProgressRender.JOIN + subRender.lines.head)
          else if (here.lines.size == 1 && subRender.primaryLineIndex == 0)
            subRender.prefix(here.lines.head + ProgressRender.JOIN)
          else
            here.subRenders(Seq(f"[${sub.progress * 100}%.1f%%] " -> subRender))
        case subs =>
          here.subRenders(subs.map { sub =>
            f"[${sub.progress * 100}%.1f%%] " ->
              sub.renderProgressWith(depthForDetail - 1)
          })
      }
    }

  def render(maxWidth: Int, maxHeight: Int): Seq[String] = {
    val progress = (0 until 10).to(LazyList).map(renderProgressWith)
      .collectFirst { case render if render.lines.size <= maxHeight => render }
      .getOrElse(renderProgressWith(1000))

    progress.lines.takeRight(maxHeight).map { line =>
      if (line.length <= maxWidth)
        line
      else {
        val avail = maxWidth - ProgressRender.ELLIPSIS.length
        val split = avail * 2 / 3
        line.take(split) + ProgressRender.ELLIPSIS +
          line.takeRight(avail - split)
      }
    }
  }

  def nonEmpty: Boolean = synchronized { subTasks.nonEmpty }

  def frame[T](f: => T): T = {
    start()
    val res = f
    end()
    res
  }

  def frame1[I, O](f: I => O): I => O =
    (i) =>
      try {
        start()
        f(i)
      } finally { end() }
}
