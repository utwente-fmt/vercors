package vct.parsers

import hre.lang.System._
import hre.tools.TimeKeeper
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated._
import vct.col.ast.GlobalDeclaration
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.util.ExpectedError
import vct.parsers.transform.{BlameProvider, JavaToCol, OriginProvider}

import scala.jdk.CollectionConverters.CollectionHasAsScala

case class ColJavaParser(override val originProvider: OriginProvider, override val blameProvider: BlameProvider) extends Parser(originProvider, blameProvider) {
  override def parse[G](stream: CharStream): ParseResult[G] = parse(stream, true)

  def parse[G](stream: CharStream, specCommentsNeeded: Boolean): ParseResult[G] = {
    try {
      val lexer = new LangJavaLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      originProvider.setTokenStream(tokens)
      val errors = expectedErrors(tokens, LangJavaLexer.EXPECTED_ERROR_CHANNEL, LangJavaLexer.VAL_EXPECT_ERROR_OPEN, LangJavaLexer.VAL_EXPECT_ERROR_CLOSE)
      val parser = new JavaParser(tokens)
      if (!specCommentsNeeded) {
        parser.specLevel = 1
      }
      val ec = errorCounter(parser, lexer, originProvider)

      val tree = parser.compilationUnit()
      ec.report()
      val decls = JavaToCol[G](originProvider, blameProvider, errors).convert(tree)
      ParseResult(decls, errors.map(_._3))
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }

  def parseExpr[G](stream: CharStream, specCommentsNeeded: Boolean): (vct.col.ast.Expr[G], Seq[ExpectedError]) = {
    try {
      val lexer = new LangJavaLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      originProvider.setTokenStream(tokens)
      val errors = expectedErrors(tokens, LangJavaLexer.EXPECTED_ERROR_CHANNEL, LangJavaLexer.VAL_EXPECT_ERROR_OPEN, LangJavaLexer.VAL_EXPECT_ERROR_CLOSE)
      val parser = new JavaParser(tokens)
      if (!specCommentsNeeded) {
        parser.specLevel = 1
      }
      val ec = errorCounter(parser, lexer, originProvider)

      val tree = parser.expr()
      ec.report()
      val decls = JavaToCol[G](originProvider, blameProvider, errors).convert(tree)
      (decls, errors.map(_._3))
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}