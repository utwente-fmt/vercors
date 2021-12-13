package vct.parsers

import hre.lang.System._
import hre.tools.TimeKeeper
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated._
import vct.col.ast.GlobalDeclaration
import vct.col.ast.stmt.decl.ProgramUnit
import vct.parsers.transform.{BlameProvider, JavaToCol, OriginProvider}

import scala.jdk.CollectionConverters.CollectionHasAsScala

case class ColJavaParser(val topLevelSpecs: Boolean) extends Parser {
  override def parse[G](stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] = {
    try {
      val lexer = new LangJavaLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val errors = expectedErrors(tokens, LangJavaLexer.EXPECTED_ERROR_CHANNEL, LangJavaLexer.VAL_EXPECT_ERROR_OPEN, LangJavaLexer.VAL_EXPECT_ERROR_CLOSE)
      val parser = new JavaParser(tokens)
      val ec = errorCounter(parser, lexer, originProvider)

      parser.specLevel = if(topLevelSpecs) 1 else 0

      val tree = parser.compilationUnit()
      ec.report()
      val decls = JavaToCol[G](originProvider, blameProvider, errors).convert(tree)
      ParseResult(decls, Nil)
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}