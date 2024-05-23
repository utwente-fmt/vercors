package hre.progress.task
import hre.progress.ProgressRender

case class SimpleNamedTask(
    superTask: AbstractTask,
    name: String,
    override val progressWeight: Option[Double] = None,
) extends Task {
  override def profilingBreadcrumb: String = name
  override def renderHere: ProgressRender = ProgressRender(name)
}
