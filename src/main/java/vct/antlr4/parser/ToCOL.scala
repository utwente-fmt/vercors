package vct.antlr4.parser

import hre.ast.FileOrigin
import hre.lang.HREExitException
import org.antlr.v4.runtime.{CommonTokenStream, Parser, ParserRuleContext}
import vct.col.ast.generic.ASTNode
import vct.col.util.ASTFactory
import hre.lang.System._

abstract class ToCOL(fileName: String, tokens: CommonTokenStream, parser: Parser) {
  val create = new ASTFactory[ParserRuleContext]()

  private def fileOrigin(tree: ParserRuleContext): FileOrigin = {
    val startLine = tree.start.getLine
    val startCol = tree.start.getCharPositionInLine
    val endLine = tree.stop.getLine
    val endCol = tree.stop.getCharPositionInLine + tree.stop.getStopIndex - tree.stop.getStartIndex

    new FileOrigin(fileName, startLine, startCol, endLine, endCol)
  }

  def origin[T <: ASTNode](tree: ParserRuleContext, node: T): T = {
    if (node.getOrigin == null) {
      node.setOrigin(fileOrigin(tree))
    }
    node
  }

  /**
   * Print notice and exit, because a rule is unimplemented in the conversion to COL. Named after the scala
   * "unimplemented" method, [[???]]
   */
  def ??(tree: ParserRuleContext): Nothing = {
    fileOrigin(tree).report("error",
      String.format("This construct (%s) is syntactically valid, but not supported by VerCors.", tree.getClass.getSimpleName))
    throw new HREExitException(1)
  }
}
