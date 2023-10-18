package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{CParser, LangCLexer}
import vct.parsers.transform.{BlameProvider, CToCol, OriginProvider}

case class ColIParser(
    override val originProvider: OriginProvider,
    override val blameProvider: BlameProvider,
) extends Parser(originProvider, blameProvider) {
  override def parse[G](stream: CharStream): ParseResult[G] = {
    try {
      val lexer = new LangCLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      originProvider.setTokenStream(tokens)
      val parser = new CParser(tokens)

      val (errors, tree) =
        noErrorsOrThrow(parser, lexer, originProvider) {
          val errors = expectedErrors(
            tokens,
            LangCLexer.EXPECTED_ERROR_CHANNEL,
            LangCLexer.VAL_EXPECT_ERROR_OPEN,
            LangCLexer.VAL_EXPECT_ERROR_CLOSE,
          )
          val tree = parser.compilationUnit()
          (errors, tree)
        }

      val decls = CToCol[G](originProvider, blameProvider, errors).convert(tree)
      ParseResult(decls, errors.map(_._3))
    } catch { case m: MatchError => throw ParseMatchError(m.getMessage()) }
  }
}
