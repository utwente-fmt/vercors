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
    val endColIdx =
      stop.getCharPositionInLine + stop.getStopIndex - stop.getStartIndex + 1
    apply(startLineIdx, endLineIdx, Some((startColIdx, endColIdx)))
  }

  def apply(startLineIdx: Int, endLineIdx: Int, cols: Option[(Int, Int)]): T =
    apply()

  def apply(): T
}

object OriginProvider {
  def apply(
      startLineIdx: Int,
      endLineIdx: Int,
      cols: Option[(Int, Int)],
  ): Origin = Origin(Seq(PositionRange(startLineIdx, endLineIdx, cols)))

  def apply(
      origin: Origin,
      startLineIdx: Int,
      endLineIdx: Int,
      cols: Option[(Int, Int)],
  ): Origin = {
    Origin(
      apply(startLineIdx, endLineIdx, cols).originContents ++
        origin.originContents
    )
  }

  def apply(start: Token, stop: Token): Origin = {
    val startLineIdx = start.getLine - 1
    val startColIdx = start.getCharPositionInLine
    val endLineIdx = stop.getLine - 1
    val endColIdx =
      stop.getCharPositionInLine + stop.getStopIndex - stop.getStartIndex + 1
    apply(startLineIdx, endLineIdx, Some((startColIdx, endColIdx)))
  }

  def apply(origin: Origin, start: Token, stop: Token): Origin = {
    Origin(apply(start, stop).originContents ++ origin.originContents)
  }

  def apply(ctx: ParserRuleContext): Origin = apply(ctx.start, ctx.stop)

  def apply(): Origin = Origin(Seq())
}

trait BlameProvider extends PositionContextProvider[Blame[VerificationFailure]]

case class ConstantBlameProvider(globalBlame: Blame[VerificationFailure])
    extends BlameProvider {
  override def apply(): Blame[VerificationFailure] = globalBlame
}
