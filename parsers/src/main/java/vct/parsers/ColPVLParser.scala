package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{LangPVLLexer, PVLParser}
import vct.col.ast.GlobalDeclaration
import vct.parsers.transform.{BlameProvider, OriginProvider}

case class ColPVLParser() extends Parser {
  override def parse(stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): Seq[GlobalDeclaration] = {
    try {
      val lexer = new LangPVLLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new PVLParser(tokens)
      val ec = errorCounter(parser, lexer)

      val tree = parser.program
      ec.report()
      // ProgramUnit pu = PVLtoCOL.convert(tree, file_name, tokens, parser);
      return null
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}