package vct.parsers

import hre.ast.FileOrigin
import hre.lang.HREExitException
import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.Contract
import vct.col.ast.util.{ASTFactory, ContractBuilder}

import java.nio.file.Paths

abstract class ToCOL(fileName: String, tokens: CommonTokenStream, parser: org.antlr.v4.runtime.Parser) {
  val create = new ASTFactory[ParserRuleContext]()

  def origin[T <: ASTNode](tree: ParserRuleContext, node: T): T = {
    if (node.getOrigin == null) {
      node.setOrigin(fileOrigin(tree))
    }
    node
  }

  def origin[T <: ASTNode](tree: ParserRuleContext, nodes: Seq[T]): Seq[T] = {
    for (node <- nodes) {
      if (node.getOrigin == null) {
        node.setOrigin(fileOrigin(tree))
      }
    }
    nodes
  }

  def getContract[T](converters: (ContractBuilder => Unit)*): Contract = {
    val builder = new ContractBuilder()
    converters.foreach(_ (builder))
    builder.getContract(false)
  }

  def flattenIfSingleStatement(statements: Seq[ASTNode]): ASTNode = statements match {
    case Seq(x) => x
    case otherwise => create block (otherwise: _*)
  }

  def getOrFail[B](node: ParserRuleContext, thing: Either[String, B]): B = thing match {
    case Left(err) => fail(node, err)
    case Right(good) => good
  }

  def fail(tree: ParserRuleContext, format: String, args: Object*): Nothing = {
    val message = String.format(format, args: _*)
    fileOrigin(tree).report("error", message)
    throw new HREExitException(1)
  }

  private def fileOrigin(tree: ParserRuleContext): FileOrigin = {
    val startLine = tree.start.getLine
    val startCol = tree.start.getCharPositionInLine + 1
    val endLine = tree.stop.getLine
    val endCol = tree.stop.getCharPositionInLine + tree.stop.getStopIndex - tree.stop.getStartIndex + 1

    new FileOrigin(Paths.get(fileName), startLine, startCol, endLine, endCol)
  }

  def getOrFail[B](node: ParserRuleContext, thing: Option[B], message: String): B = thing match {
    case None => fail(node, message)
    case Some(b) => b
  }

  def failIfDefined[T <: ParserRuleContext](node: Option[T], format: String, args: Object*): Unit = node match {
    case Some(node) => fail(node, format, args)
    case None => // do nothing
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
