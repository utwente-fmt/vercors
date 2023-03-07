package hre.progress.task

case class UpdateableTask(superTask: Task) extends Task {
  var currentName: Option[String] = None

  override def profilingBreadcrumb: String = currentName.get
  override def progressText: String = currentName.get

  def scope[T](f: (String => Unit) => T): T =
    try {
      f(update)
    } finally {
      if(currentName.isDefined) end()
    }

  private def update(name: String): Unit = {
    if(currentName.isDefined) end()
    currentName = Some(name)
    start()
  }
}
