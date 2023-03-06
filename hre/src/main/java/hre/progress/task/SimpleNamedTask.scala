package hre.progress.task

case class SimpleNamedTask(superTask: Task, name: String) extends Task {
  override def profilingBreadcrumb: String = name
  override def progressText: String = name
}
