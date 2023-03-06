package hre.progress.task

case class UpdateableTask(superTask: Task) extends Task {
  var currentName: Option[String] = None

  override def profilingBreadcrumb: String = currentName.get
  override def progressText: String = currentName.get

  def scope(f: => Unit): Unit =
    try {
      f
    } finally {
      if(currentName.isDefined) end()
    }

  def update(name: String): Unit = {
    if(currentName.isDefined) end()
    currentName = Some(name)
    start()
  }
}
