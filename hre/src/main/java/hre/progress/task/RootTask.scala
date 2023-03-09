package hre.progress.task
import hre.progress.TaskRegistry

case class RootTask() extends AbstractTask {
  override def superTaskOrRoot: Option[Task] = None
  override def profilingBreadcrumb: String = "root"
  override def progressText: String = "VerCors"

  override def end(): Unit = {
    poll()
    val stack = TaskRegistry.threadTaskStack.get()
    if(stack.lastOption.contains(this)) {
      stack.remove(stack.length - 1)
    }
  }
}
