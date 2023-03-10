package hre.progress.task

case class UpdateableTask(superTask: AbstractTask, approxUpdates: Option[Int] = None) extends Task {
  private var currentName: Option[String] = None
  private var updatesDone = 0

  override def progressWeight: Option[Double] =
    approxUpdates.map(approxUpdates => if(updatesDone < approxUpdates) 1.0 / approxUpdates else 0.0)

  override def profilingBreadcrumb: String = currentName.get
  override def progressText: String = currentName.get

  def scope[T](f: (String => Unit) => T): T =
    try {
      f(update)
    } finally {
      if(currentName.isDefined) end()
    }

  private def update(name: String): Unit = {
    if(currentName.isDefined) {
      end()
      progressDone = 0.0
    }
    updatesDone += 1
    currentName = Some(name)
    start()
  }
}
