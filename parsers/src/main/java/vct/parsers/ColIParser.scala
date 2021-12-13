package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{CParser, LangCLexer}
import vct.col.ast.GlobalDeclaration
import vct.parsers.transform.{BlameProvider, CToCol, OriginProvider}

case class ColIParser() extends Parser {
  override def parse[G](stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] = {
    try {
      val lexer = new LangCLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val errors = expectedErrors(tokens, LangCLexer.EXPECTED_ERROR_CHANNEL, LangCLexer.VAL_EXPECT_ERROR_OPEN, LangCLexer.VAL_EXPECT_ERROR_CLOSE)
      val parser = new CParser(tokens)
      val ec = errorCounter(parser, lexer, originProvider)
      val tree = parser.compilationUnit()
      ec.report()
      val decls = CToCol[G](originProvider, blameProvider, errors).convert(tree)
      ParseResult(decls, Nil)
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}