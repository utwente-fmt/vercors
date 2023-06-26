package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{LangPVLLexer, PVLParser}
import vct.col.ast.GlobalDeclaration
import vct.parsers.transform.{BlameProvider, OriginProvider, PVLToCol}

case class ColPVLParser(override val originProvider: OriginProvider, override val blameProvider: BlameProvider) extends Parser(originProvider, blameProvider) {
  override def parse[G](stream: CharStream): ParseResult[G] = {
    try {
      val lexer = new LangPVLLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      originProvider.setTokenStream(tokens)
      val parser = new PVLParser(tokens)

      val (errors, tree) = noErrorsOrThrow(parser, lexer, originProvider) {
        val errors = expectedErrors[G](tokens, LangPVLLexer.EXPECTED_ERROR_CHANNEL, LangPVLLexer.VAL_EXPECT_ERROR_OPEN, LangPVLLexer.VAL_EXPECT_ERROR_CLOSE)
        val tree = parser.program()
        (errors, tree)
      }

      val decls = PVLToCol[G](originProvider, errors._2).convert(tree)
      ParseResult(errors._2.map(_._3) ++ errors._1 ++ decls)
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}