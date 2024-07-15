package hre.progress.task
import hre.progress.{ProgressRender, TaskRegistry}

case class RootTask() extends AbstractTask {
  override def superTaskOrRoot: Option[Task] = None
  override def profilingBreadcrumb: String = "root"
  override def renderHere: ProgressRender = ProgressRender("VerCors")

  override def end(): Unit = {
    poll()
    val stack = TaskRegistry.get.threadTaskStack.get()
    if (stack.lastOption.contains(this)) { stack.remove(stack.length - 1) }
  }
}
