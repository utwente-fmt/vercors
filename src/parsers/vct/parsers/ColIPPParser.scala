package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{CPPParser, LangCPPLexer}
import vct.col.origin.Origin
import vct.parsers.transform.{BlameProvider, CPPToCol, OriginProvider}

case class ColIPPParser(override val origin: Origin, override val blameProvider: BlameProvider) extends Parser(origin, blameProvider) {

  override def parse[G](stream: CharStream): ParseResult[G] = {
    try {
      val lexer = new LangCPPLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new CPPParser(tokens)

      val (errors, tree) = noErrorsOrThrow(origin, parser, lexer) {
        val errors = expectedErrors(tokens, LangCPPLexer.EXPECTED_ERROR_CHANNEL, LangCPPLexer.VAL_EXPECT_ERROR_OPEN, LangCPPLexer.VAL_EXPECT_ERROR_CLOSE)
        val tree = parser.translationUnit()
        (errors, tree)
      }

      val decls = CPPToCol[G](origin, blameProvider, errors).convert(tree)
      ParseResult(decls, errors.map(_._3))
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}