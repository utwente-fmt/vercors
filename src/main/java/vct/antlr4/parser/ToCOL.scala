package vct.antlr4.parser

import hre.ast.FileOrigin
import hre.lang.HREExitException
import org.antlr.v4.runtime.{CommonTokenStream, Parser, ParserRuleContext}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.Contract
import vct.col.ast.util.ContractBuilder
import vct.col.util.ASTFactory

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

  def origin[T <: ASTNode](tree: ParserRuleContext, nodes: Seq[T]): Seq[T] = {
    for(node <- nodes) {
      if(node.getOrigin == null) {
        node.setOrigin(fileOrigin(tree))
      }
    }
    nodes
  }

  def getContract[T](tree: Option[T], converter: (T, ContractBuilder) => Unit): Contract = {
    val builder = new ContractBuilder()
    tree match {
      case Some(node) => converter(node, builder)
      case None => // leave contract empty
    }
    builder.getContract
  }

  def fail(tree: ParserRuleContext, format: String, args: Object*): Nothing = {
    val message = String.format(format, args:_*)
    fileOrigin(tree).report("error", message)
    throw new HREExitException(1)
  }

  /**
   * Print notice and exit, because a rule is unimplemented in the conversion to COL. Named after the scala
   * "unimplemented" method, [[???]]
   */
  def ??(tree: ParserRuleContext): Nothing = {
    fail(tree,
      "This construct (%s) is syntactically valid, but not supported by VerCors.",
      tree.getClass.getSimpleName)
  }
}
