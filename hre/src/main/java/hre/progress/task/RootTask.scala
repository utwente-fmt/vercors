package hre.progress.task
import hre.progress.TaskRegistry

import scala.util.chaining.scalaUtilChainingOps

case class RootTask() extends Task {
  override def superTask: Task = null
  override def profilingBreadcrumb: String = "root"
  override def progressText: String = "VerCors"
  override def profilingTrail: Seq[String] = Seq(profilingBreadcrumb)

  override def start(): Unit = {
    startUsage = Some(TaskRegistry.ownUsage())
    TaskRegistry.reportUsage(startUsage.get, Seq("<untracked>"))
    TaskRegistry.threadTaskStack.get() += this
  }

  override def end(): Unit = {
    poll()
    TaskRegistry.threadTaskStack.get().pipe(s => s.remove(s.length - 1))
  }
}
