package hre.progress.task
import hre.progress.ProgressRender

case class NameSequenceTask(superTask: AbstractTask, var names: Seq[String], var progressWeights: Seq[Double] = Nil) extends Task {
  override def profilingBreadcrumb: String = names.head
  override def renderHere: ProgressRender = ProgressRender(names.head)

  override def progressWeight: Option[Double] = progressWeights.headOption

  def scope[T](f: (() => Unit) => T): T = {
    start()
    val res = f(next)
    end()
    res
  }

  private def next(): Unit = {
    end()
    names = names.tail
    progressWeights = if(progressWeights.isEmpty) Nil else progressWeights.tail
    progressDone = 0.0
    start()
  }
}
