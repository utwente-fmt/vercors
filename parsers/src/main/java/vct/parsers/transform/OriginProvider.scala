package vct.parsers.transform

import org.antlr.v4.runtime.{CommonTokenStream, Lexer, ParserRuleContext, Token}
import vct.col.ast._
import vct.col.origin._
import vct.col.util.ExpectedError

import java.nio.file.Path

trait OriginProvider {
  def apply(ctx: ParserRuleContext): Origin = this(ctx.start, ctx.stop)
  def apply(start: Token, stop: Token): Origin
}

abstract class BlameProvider {
  def apply(ctx: ParserRuleContext): Blame[VerificationFailure] = this(ctx.start, ctx.stop)
  def apply(start: Token, stop: Token): Blame[VerificationFailure]
}

case class FileOriginProvider(path: Path) extends BlameProvider with OriginProvider {
  override def apply(start: Token, stop: Token): FileOrigin = {
    val startLine = start.getLine - 1
    val startCol = start.getCharPositionInLine
    val endLine = stop.getLine - 1
    val endCol = stop.getCharPositionInLine + stop.getStopIndex - stop.getStartIndex + 1

    FileOrigin(path, startLine, startCol, endLine, endCol)
  }

  override def apply(ctx: ParserRuleContext): FileOrigin = this(ctx.start, ctx.stop)
}

case class InterpretedFileOriginProvider(tokens: CommonTokenStream, originalPath: Path, interpretedPath: Path) extends BlameProvider with OriginProvider {
  private def getLineOffset(token: Token): Option[Int] = {
    for(tokIdx <- token.getTokenIndex-1 to 0 by -1) {
      val markerToken = tokens.get(tokIdx)
      if(markerToken.getChannel == 2) {
        val lineDirectiveLine = Integer.parseInt(markerToken.getText.split(' ')(1))
        val tokenLine = markerToken.getLine
        // FIXME PB: check for off-by-one stuff
        return Some(lineDirectiveLine - tokenLine - 1)
      }
    }

    None
  }

  override def apply(start: Token, stop: Token): InputOrigin = {
    val startLine = start.getLine - 1
    val startCol = start.getCharPositionInLine
    val endLine = stop.getLine - 1
    val endCol = stop.getCharPositionInLine + stop.getStopIndex - stop.getStartIndex + 1

    (getLineOffset(start), getLineOffset(stop)) match {
      case (Some(startOffset), Some(endOffset)) =>
        InterpretedOrigin(interpretedPath, startLine, startCol, endLine, endCol, originalPath,
          startLine+startOffset, endLine+endOffset)
      case _ => FileOrigin(interpretedPath, startLine, startCol, endLine, endCol)
    }
  }

  override def apply(ctx: ParserRuleContext): InputOrigin = this(ctx.start, ctx.stop)
}