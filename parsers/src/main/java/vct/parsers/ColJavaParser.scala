package vct.parsers

import hre.lang.System._
import hre.tools.TimeKeeper
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated._
import vct.col.ast.GlobalDeclaration
import vct.col.ast.stmt.decl.ProgramUnit
import vct.parsers.transform.{BlameProvider, JavaToCol, OriginProvider}

case class ColJavaParser(val topLevelSpecs: Boolean) extends Parser {
  override def parse(stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): Seq[GlobalDeclaration] = {
    try {
      val lexer = new LangJavaLexer(stream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new JavaParser(tokens)
      val ec = errorCounter(parser, lexer, originProvider)

      parser.specLevel = if(topLevelSpecs) 1 else 0

      val tree = parser.compilationUnit()
      ec.report()
      JavaToCol(originProvider, blameProvider).convert(tree)
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}