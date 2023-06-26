package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated._
import vct.col.ast.GlobalDeclaration
import vct.parsers.transform.{BlameProvider, JavaToCol, OriginProvider}

import scala.jdk.CollectionConverters.CollectionHasAsScala

case class ColJavaParser(override val originProvider: OriginProvider, override val blameProvider: BlameProvider) extends Parser(originProvider, blameProvider) {
  override def parse[G](stream: CharStream): ParseResult[G] = parse(stream, true)

  def parse[G](stream: CharStream, specCommentsNeeded: Boolean): ParseResult[G] = {
    try {
      val lexer = new LangJavaLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      originProvider.setTokenStream(tokens)
      val parser = new JavaParser(tokens)
      /* This is needed for JavaBIP, to allow Perm at the top level.
         If changed, this should also be made consistent with the parser below. */
      if (!specCommentsNeeded) {
        parser.specLevel = 1
      }

      val (errors, tree) = noErrorsOrThrow(parser, lexer, originProvider) {
        val errors = expectedErrors[G](tokens, LangJavaLexer.EXPECTED_ERROR_CHANNEL, LangJavaLexer.VAL_EXPECT_ERROR_OPEN, LangJavaLexer.VAL_EXPECT_ERROR_CLOSE)
        val tree = parser.compilationUnit()
        (errors, tree)
      }

      val decls = JavaToCol[G](originProvider, errors._2).convert(tree)
      ParseResult(errors._2.map(_._3) ++ errors._1 ++ decls)
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }

  def parseExpr[G](stream: CharStream, specCommentsNeeded: Boolean): vct.col.ast.Expr[G] = {
    try {
      val lexer = new LangJavaLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      originProvider.setTokenStream(tokens)
      val parser = new JavaParser(tokens)
      if (!specCommentsNeeded) {
        parser.specLevel = 1
      }

      val (errors, tree) = noErrorsOrThrow(parser, lexer, originProvider) {
        val errors = expectedErrors[G](tokens, LangJavaLexer.EXPECTED_ERROR_CHANNEL, LangJavaLexer.VAL_EXPECT_ERROR_OPEN, LangJavaLexer.VAL_EXPECT_ERROR_CLOSE)
        val tree = parser.expr()
        (errors, tree)
      }

      val expr = JavaToCol[G](originProvider, errors._2).convert(tree)

      if(errors._2.nonEmpty) {
        val (start, stop, _) = errors._2.head
        throw ParseError(originProvider(start, stop), "Expected errors in recursively parsed embedded expressions are currently not supported.")
      }

      expr
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}