package vct.parsers.transform

import hre.io.Readable
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext, Token}
import vct.antlr4.generated.LangCLexer
import vct.col.origin._

trait PositionContextProvider[T] {
  def apply(ctx: ParserRuleContext): T = this(ctx.start, ctx.stop)

  def apply(start: Token, stop: Token): T = {
    val startLineIdx = start.getLine - 1
    val startColIdx = start.getCharPositionInLine
    val endLineIdx = stop.getLine - 1
    val endColIdx = stop.getCharPositionInLine + stop.getStopIndex - stop.getStartIndex + 1
    apply(startLineIdx, endLineIdx, Some((startColIdx, endColIdx)))
  }

  def apply(startLineIdx: Int, endLineIdx: Int, cols: Option[(Int, Int)]): T = apply()

  def apply(): T
}

object OriginProvider extends PositionContextProvider[Origin] {
  override def apply(startLineIdx: Int, endLineIdx: Int, cols: Option[(Int, Int)]): Origin =
    Origin(Seq(StartEndLines(startLineIdx, endLineIdx))).addOriginCols(cols)

  override def apply(): Origin = Origin(Seq())
}

object ReadableOriginProvider extends PositionContextProvider[Origin] {
  def apply(readable: Readable): Origin =
    Origin(Seq()).addReadableOrigin(readable)

  override def apply(): Origin = Origin(Seq())
}

object RedirectOriginProvider extends PositionContextProvider[Origin] {
  def apply(origin: Origin, textualOrigin: String): Unit = {

  }

  override def apply(): Origin = ???
}

trait BlameProvider extends PositionContextProvider[Blame[VerificationFailure]]

case class ConstantBlameProvider(globalBlame: Blame[VerificationFailure]) extends BlameProvider {
  override def apply(): Blame[VerificationFailure] = globalBlame
}
