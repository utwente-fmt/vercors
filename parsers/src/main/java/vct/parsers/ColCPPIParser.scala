package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{CPPParser, LangCPPLexer}
import vct.parsers.transform.{BlameProvider, CPPToCol, OriginProvider}

case class ColCPPIParser(override val originProvider: OriginProvider, override val blameProvider: BlameProvider) extends Parser(originProvider, blameProvider) {
  override def parse[G](stream: CharStream): ParseResult[G] = {
    try {
      val lexer = new LangCPPLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      originProvider.setTokenStream(tokens)
      val errors = expectedErrors(tokens, LangCPPLexer.EXPECTED_ERROR_CHANNEL, LangCPPLexer.VAL_EXPECT_ERROR_OPEN, LangCPPLexer.VAL_EXPECT_ERROR_CLOSE)
      val parser = new CPPParser(tokens)
      val ec = errorCounter(parser, lexer, originProvider)
      val tree = parser.translationUnit()
      ec.report()
      val decls = CPPToCol[G](originProvider, blameProvider, errors).convert(tree)
      ParseResult(decls, errors.map(_._3))
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}



