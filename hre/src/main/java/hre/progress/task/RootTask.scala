package hre.progress.task
import hre.progress.TaskRegistry

case class RootTask() extends Task {
  override def superTask: Task = null
  override def profilingBreadcrumb: String = "root"
  override def progressText: String = "VerCors"
  override def profilingTrail: Seq[String] = Seq(profilingBreadcrumb)

  override def start(): Unit = superTask.synchronized {
    startUsage = TaskRegistry.ownUsage()
    TaskRegistry.reportUsage(startUsage, Seq("<untracked>"))
  }

  override def end(): Unit = poll()
}
