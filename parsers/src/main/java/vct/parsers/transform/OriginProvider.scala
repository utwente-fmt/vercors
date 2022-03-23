package vct.parsers.transform

import hre.io.Readable
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext, Token}
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

abstract class OriginProvider extends PositionContextProvider[Origin] {
  private var _tokenStream: Option[CommonTokenStream] = None

  def setTokenStream(tokenStream: CommonTokenStream): Unit = _tokenStream = Some(tokenStream)
  def tokenStream: CommonTokenStream = _tokenStream.get
}
trait BlameProvider extends PositionContextProvider[Blame[VerificationFailure]]

case class ConstantBlameProvider(globalBlame: Blame[VerificationFailure]) extends BlameProvider {
  override def apply(): Blame[VerificationFailure] = globalBlame
}

case class ReadableOriginProvider(readable: Readable) extends OriginProvider {
  override def apply(startLineIdx: Int, endLineIdx: Int, cols: Option[(Int, Int)]): ReadableOrigin = {
    ReadableOrigin(readable, startLineIdx, endLineIdx, cols)
  }

  override def apply(): Origin = FileSpanningOrigin
}

case class InterpretedFileOriginProvider(original: OriginProvider, interpreted: Readable) extends OriginProvider {
  private def getLineOffset(lineIdx: Int): Option[Int] = {
    val firstTokenAtOrPastLine = (0 until tokenStream.size()).find(i => tokenStream.get(i).getLine-1 >= lineIdx).getOrElse(return None)

    for(tokIdx <- firstTokenAtOrPastLine to 0 by -1) {
      val markerToken = tokenStream.get(tokIdx)
      if(markerToken.getChannel == 2) {
        val lineDirectiveLine = Integer.parseInt(markerToken.getText.split(' ')(1))
        val tokenLine = markerToken.getLine
        // FIXME PB: check for off-by-one stuff
        return Some(lineDirectiveLine - tokenLine - 1)
      }
    }

    None
  }

  override def apply(startLineIdx: Int, endLineIdx: Int, cols: Option[(Int, Int)]): InputOrigin = {
    (getLineOffset(startLineIdx), getLineOffset(endLineIdx)) match {
      case (Some(startOffset), Some(endOffset)) =>
        InterpretedOrigin(interpreted, startLineIdx, endLineIdx, cols, original(startLineIdx+startOffset, endLineIdx+endOffset, None))
      case _ =>
        ReadableOrigin(interpreted, startLineIdx, endLineIdx, cols)
    }
  }

  override def apply(): Origin = FileSpanningOrigin
}