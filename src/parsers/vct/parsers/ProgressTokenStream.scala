package vct.parsers

import hre.progress.task.AbstractTask
import hre.progress.{Progress, ProgressRender, TaskRegistry}
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.{BufferedTokenStream, RuleContext, Token, TokenSource, TokenStream}
import vct.col.origin.Origin
import vct.parsers.transform.OriginProvider

object ProgressTokenStream {
  class Task(override val superTask: AbstractTask, baseOrigin: Origin) extends hre.progress.task.Task {
    var token: Option[Token] = None

    override def profilingBreadcrumb: String =
      token match {
        case None =>
          "Unknown"
        case Some(token) =>
          val o = OriginProvider(baseOrigin, token, token)
          o.shortPositionText
      }

    override def renderHere: ProgressRender =
      token match {
        case None =>
          ProgressRender("Unknown")
        case Some(token) =>
          val o = OriginProvider(baseOrigin, token, token)
          o.renderProgress(s"At ${o.shortPositionText}", short = false)
      }

    override def renderHereShort: ProgressRender = ProgressRender(profilingBreadcrumb)
  }

  def apply[T](baseOrigin: Origin, stream: BufferedTokenStream)(f: TokenStream => T): T =
    Progress.stages(Seq("Lexing" -> 1, "Parsing" -> 3)) { nextStage =>
      stream.fill()
      nextStage()
      val task = new Task(TaskRegistry.currentTaskInThread, baseOrigin)
      val progressStream = new ProgressTokenStream(stream, task)

      task.frame(f(progressStream))
    }
}

private class ProgressTokenStream(stream: BufferedTokenStream, task: ProgressTokenStream.Task) extends TokenStream {

  private var maxIndex = 0

  private def update[T](f: => T): T = {
    val result = f

    if(stream.index() > maxIndex)
      maxIndex = stream.index()

    result
  }

  override def LT(k: Int): Token = update(stream.LT(k))
  override def get(index: Int): Token = update(stream.get(index))
  override def getTokenSource: TokenSource = stream.getTokenSource
  override def getText(interval: Interval): String = stream.getText(interval)
  override def getText: String = stream.getText
  override def getText(ctx: RuleContext): String = stream.getText(ctx)
  override def getText(start: Token, stop: Token): String = stream.getText(start, stop)
  override def consume(): Unit = update(stream.consume())
  override def LA(i: Int): Int = update(stream.LA(i))
  override def mark(): Int = stream.mark()
  override def release(marker: Int): Unit = stream.release(marker)
  override def index(): Int = stream.index()
  override def seek(index: Int): Unit = update(stream.seek(index))
  override def size(): Int = stream.size()
  override def getSourceName: String = stream.getSourceName
}
