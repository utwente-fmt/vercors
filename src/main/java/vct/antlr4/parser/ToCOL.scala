package vct.antlr4.parser

import hre.ast.FileOrigin
import org.antlr.v4.runtime.{CommonTokenStream, Parser, ParserRuleContext}
import vct.col.ast.generic.ASTNode
import vct.col.util.ASTFactory

abstract class ToCOL(fileName: String, tokens: CommonTokenStream, parser: Parser) {
  val create = new ASTFactory[ParserRuleContext]()

  def origin[T <: ASTNode](tree: ParserRuleContext, node: T): T = {
    if (node.getOrigin == null) {
      val startLine = tree.start.getLine
      val startCol = tree.start.getCharPositionInLine
      val endLine = tree.stop.getLine
      val endCol = tree.stop.getCharPositionInLine + tree.stop.getStopIndex - tree.stop.getStartIndex

      val origin = new FileOrigin(fileName, startLine, startCol, endLine, endCol)
      node.setOrigin(origin)
    }
    node
  }
}
