package vct.parsers

import com.typesafe.scalalogging.LazyLogging
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{CPPParser, LangCPPLexer}
import vct.col.origin.Origin
import vct.parsers.transform.{BlameProvider, CPPToCol}

case class ColIPPParser(override val origin: Origin, override val blameProvider: BlameProvider, val cppOrigin: Option[Origin]) extends Parser(origin, blameProvider)with LazyLogging {

  override def parse[G](stream: CharStream): ParseResult[G] = {
    try {
      logger.debug(s"STARTING LEXING")
      val lexer = new LangCPPLexer(stream)
      logger.debug(s"STARTING TOKEN STREAM")
      val tokens = new CommonTokenStream(lexer)
      logger.debug(s"STARTING PARSING")
      val parser = new CPPParser(tokens)
      logger.debug(s"DONE PARSING")

      val (errors, tree) = noErrorsOrThrow(origin, parser, lexer) {
        logger.debug(s"DONE noErrorsOrThrow")
        val errors = expectedErrors(tokens, LangCPPLexer.EXPECTED_ERROR_CHANNEL, LangCPPLexer.VAL_EXPECT_ERROR_OPEN, LangCPPLexer.VAL_EXPECT_ERROR_CLOSE)
        logger.debug(s"DONE expectedErrors")
        val tree = parser.translationUnit()
        logger.debug(s"DONE translationUnit")
        (errors, tree)
      }
      logger.debug(s"STARTING CPPTOCOL")
      val decls = CPPToCol[G](origin, blameProvider, errors, cppOrigin.map(o => (tokens, o))).convert(tree)
      logger.debug(s"DONE CPPTOCOL")
      ParseResult(decls, errors.map(_._3))
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }
}