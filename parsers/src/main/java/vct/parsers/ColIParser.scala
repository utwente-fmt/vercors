package vct.parsers

import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{CParser, LangCLexer}
import vct.col.ast.GlobalDeclaration
import vct.parsers.transform.{BlameProvider, CToCol, OriginProvider}

case class ColIParser() extends Parser {
  override def parse(stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): Seq[GlobalDeclaration] = {
    try {
      val lexer = new LangCLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new CParser(tokens)
      val ec = errorCounter(parser, lexer)

      val tree = parser.compilationUnit()
      ec.report()
      CToCol(originProvider, blameProvider).convert(tree)
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}