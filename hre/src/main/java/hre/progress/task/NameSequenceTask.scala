package hre.progress.task

case class NameSequenceTask(superTask: AbstractTask, var names: Seq[String], var progressWeights: Seq[Double] = Nil) extends Task {
  override def profilingBreadcrumb: String = names.head
  override def progressText: String = names.head

  override def progressWeight: Option[Double] = progressWeights.headOption

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
    progressWeights = if(progressWeights.isEmpty) Nil else progressWeights.tail
    progressDone = 0.0
    start()
  }
}
