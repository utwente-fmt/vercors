package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{LangPVLLexer, PVLParser}
import vct.col.ast.GlobalDeclaration
import vct.col.origin.Origin
import vct.parsers.transform.{BlameProvider, OriginProvider, PVLToCol}

case class ColPVLParser(override val origin: Origin,
                        override val blameProvider: BlameProvider) extends Parser(origin, blameProvider) {
  override def parse[G](stream: CharStream): ParseResult[G] = {
    try {
      val lexer = new LangPVLLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new PVLParser(tokens)

      val (errors, tree) = noErrorsOrThrow(origin, parser, lexer) {
        val errors = expectedErrors(tokens, LangPVLLexer.EXPECTED_ERROR_CHANNEL,
          LangPVLLexer.VAL_EXPECT_ERROR_OPEN, LangPVLLexer.VAL_EXPECT_ERROR_CLOSE)
        val tree = parser.program()
        (errors, tree)
      }

      val decls = PVLToCol[G](origin, blameProvider, errors).convert(tree)
      ParseResult(decls, errors.map(_._3))
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}