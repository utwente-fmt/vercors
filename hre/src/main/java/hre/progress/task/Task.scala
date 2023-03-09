package hre.progress.task

abstract class Task extends AbstractTask {
  def superTask: AbstractTask
  override def superTaskOrRoot: Option[AbstractTask] = Some(superTask)
}
