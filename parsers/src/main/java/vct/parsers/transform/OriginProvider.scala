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

case class RedirectOrigin(o: Origin, sr: ReadableOriginProvider, start: Int, end: Int, cols: Option[(Int, Int)]) extends Origin {
  override def preferredName: String = o.preferredName

  override def context: String = o match {
    case ReadableOrigin(readable, baseStart, baseEnd, baseCols) => {}
      val s = baseStart + start
      val e = baseEnd + end
      val c: Option[(Int, Int)] = (baseCols, cols) match {
        // TODO (RR): If inner startline == basestartline, then probably bS should be added to e as well. Right now it's probably incorrect!
        case (Some((bS, bE)), Some((s, e))) => Some((bS + s, bE + e))
        case (None, cols) => cols
      }
      ReadableOrigin(readable, s, e, c).context
    case o: Origin =>
      s"== Within the following context ==\n${o.context}\n== Specifically ==\n${sr.apply(start, end, cols).context}"
  }
}

case class RedirectOriginProvider(o: Origin, sr: ReadableOriginProvider) extends OriginProvider {
  override def apply(startLineIdx: Int, endLineIdx: Int, cols: Option[(Int, Int)]): RedirectOrigin = {
    RedirectOrigin(o, sr, startLineIdx, endLineIdx, cols)
  }

  override def apply(): Origin = FileSpanningOrigin
}

case class InterpretedFileOriginProvider(original: OriginProvider, interpreted: Readable) extends OriginProvider {
  private def getLineOffset(lineIdx: Int): Option[Int] = {
    val firstTokenAtOrPastLine = (0 until tokenStream.size()).find(i => tokenStream.get(i).getLine-1 >= lineIdx).getOrElse(return None)

    for(tokIdx <- firstTokenAtOrPastLine to 0 by -1) {
      val markerToken = tokenStream.get(tokIdx)
      if(markerToken.getChannel == LangCLexer.LINE_DIRECTIVE_CHANNEL) {
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