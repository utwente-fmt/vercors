package hre.progress.task

case class SimpleNamedTask(superTask: AbstractTask, name: String, override val progressWeight: Option[Double] = None) extends Task {
  override def profilingBreadcrumb: String = name
  override def progressText: String = name
}
