package hre.progress.task

case class NameSequenceTask(superTask: AbstractTask, var names: Seq[String]) extends Task {
  override def profilingBreadcrumb: String = names.head
  override def progressText: String = names.head

  def scope[T](f: (() => Unit) => T): T = {
    start()
    try {
      f(next)
    } finally {
      end()
    }
  }

  private def next(): Unit = {
    end()
    names = names.tail
    start()
  }
}
