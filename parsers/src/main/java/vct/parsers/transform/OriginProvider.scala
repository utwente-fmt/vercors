package vct.parsers.transform

import org.antlr.v4.runtime.{CommonTokenStream, Lexer, ParserRuleContext}
import vct.col.ast._

import java.nio.file.Path

trait OriginProvider {
  def apply(node: ParserRuleContext): Origin
}

trait BlameProvider {
  def apply(node: ParserRuleContext): Scapegoat
}

case class FileOriginProvider(path: Path) extends OriginProvider with BlameProvider {
  override def apply(ctx: ParserRuleContext): FileOrigin = {
    val startLine = ctx.start.getLine - 1
    val startCol = ctx.start.getCharPositionInLine
    val endLine = ctx.stop.getLine - 1
    val endCol = ctx.stop.getCharPositionInLine + ctx.stop.getStopIndex - ctx.stop.getStartIndex + 1

    FileOrigin(path, startLine, startCol, endLine, endCol)
  }
}

case class InterpretedFileOriginProvider(tokens: CommonTokenStream, originalPath: Path, interpretedPath: Path) extends OriginProvider with BlameProvider {
  override def apply(ctx: ParserRuleContext): InputOrigin = {
    val startLine = ctx.start.getLine - 1
    val startCol = ctx.start.getCharPositionInLine
    val endLine = ctx.stop.getLine - 1
    val endCol = ctx.stop.getCharPositionInLine + ctx.stop.getStopIndex - ctx.stop.getStartIndex + 1

    // Search backward from the starting token for a line marker: the source the line was interpreted from.
    // See also: https://gcc.gnu.org/onlinedocs/gcc-3.4.6/cpp/Preprocessor-Output.html
    for(tokIdx <- ctx.start.getTokenIndex-1 to 0 by -1) {
      val token = tokens.get(tokIdx)
      if(token.getChannel == 2) {
        val lineDirectiveLine = Integer.parseInt(token.getText.split(' ')(1))
        val tokenLine = token.getLine
        // FIXME PB: check for off-by-one stuff
        val interpretedOffset = lineDirectiveLine - tokenLine - 1

        return InterpretedOrigin(interpretedPath, startLine, startCol, endLine, endCol, originalPath, startLine+interpretedOffset, endLine+interpretedOffset)
      }
    }

    FileOrigin(interpretedPath, startLine, startCol, endLine, endCol)
  }
}