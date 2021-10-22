package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{LangPVLLexer, PVLParser}
import vct.col.ast.GlobalDeclaration
import vct.parsers.transform.{BlameProvider, OriginProvider, PVLToCol}

case class ColPVLParser() extends Parser {
  override def parse(stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult = {
    try {
      val lexer = new LangPVLLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val errors = expectedErrors(tokens, LangPVLLexer.EXPECTED_ERROR_CHANNEL, LangPVLLexer.VAL_EXPECT_ERROR_OPEN, LangPVLLexer.VAL_EXPECT_ERROR_CLOSE)
      val parser = new PVLParser(tokens)
      val ec = errorCounter(parser, lexer, originProvider)

      val tree = parser.program()
      val decls = PVLToCol(originProvider, blameProvider, errors).convert(tree)
      ParseResult(decls, Nil)
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}