package vct.parsers
import org.antlr.v4.runtime
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, Token, TokenStream}
import vct.col.ast.GlobalDeclaration
import vct.col.origin.{Blame, ExpectedError, Origin, ReadableOrigin, VerificationFailure}
import vct.parsers.debug.DebugOptions
import vct.parsers.err.{FileNotFound, ParseError, ParseErrorStrategy, ParseErrors, ParseMatchError}
import vct.parsers.transform.{BlameProvider, OriginProvider, ToCol}
import vct.result.VerificationError.UserError
import hre.io.Readable

import java.io.{FileNotFoundException, Reader}
import java.nio.file.NoSuchFileException
import scala.jdk.CollectionConverters.CollectionHasAsScala

abstract class AntlrParser extends Parser {
  type Parser <: runtime.Parser
  type Lexer <: runtime.Lexer
  type MainContext <: runtime.ParserRuleContext
  type Converter[_] <: ToCol[_]

  def newLexer(charStream: CharStream): Lexer
  def expectedErrorChannel: Int
  def expectedErrorStart: Int
  def expectedErrorEnd: Int
  def newParser(tokenStream: TokenStream): Parser
  def parseMain(parser: Parser): MainContext
  def newConverter[G](baseOrigin: Origin, expectedErrors: Seq[(Token, Token, ExpectedError)], tokenStream: TokenStream): Converter[G]
  def mainToResult[G](converter: Converter[G], main: MainContext): Seq[GlobalDeclaration[_ <: Any]]

  def blameProvider: BlameProvider
  def debugOptions: DebugOptions

  case class UnbalancedExpectedError(tok: Token) extends UserError {
    override def code: String = "unbalancedExpectedError"
    override def text: String = "There is no scope to close here."
  }

  def collectParseErrors(lexer: Lexer, parser: Parser, baseOrigin: Origin): CollectingErrorListener = {
    parser.setErrorHandler(ParseErrorStrategy())
    parser.removeErrorListeners()
    lexer.removeErrorListeners()
    val ec = CollectingErrorListener(baseOrigin, debugOptions)
    parser.addErrorListener(ec)
    lexer.addErrorListener(ec)
    ec
  }

  def getExpectedErrors(lexer: CommonTokenStream, baseOrigin: Origin): Seq[(Token, Token, ExpectedError)] = {
    lexer.fill()
    var startStack: Seq[(String, Token)] = Nil
    var errors = Seq.empty[(Token, Token, ExpectedError)]

    for(token <- lexer.getTokens.asScala.filter(_.getChannel == expectedErrorChannel)) {
      if(token.getType == expectedErrorStart) {
        val code = token.getText
          .replace("/*", "")
          .replace("*/", "")
          .replace("[/expect", "")
          .replace("]", "")
          .strip()
        startStack :+= (code, token)
      }

      if(token.getType == expectedErrorEnd) {
        if(startStack.isEmpty) throw UnbalancedExpectedError(token)
        val (code, start) = startStack.last
        startStack = startStack.init
        val err = ExpectedError(code, OriginProvider(baseOrigin, start, token), blameProvider(start, token))
        errors :+= (start, token, err)
      }
    }

    errors.sortBy(_._1.getTokenIndex)
  }

  protected def getErrorsFor[T](origin: Origin, parser: runtime.Parser, lexer: runtime.Lexer)(f: => T): Either[Seq[ParseError], T] = Right(f)
  protected def noErrorsOrThrow[T](origin: Origin, parser: runtime.Parser, lexer: runtime.Lexer)(f: => T): T = f

  def parseAntlrStreamOrThrow[Ctx, G, Out](stream: runtime.CharStream, baseOrigin: Origin = Origin(Nil))(parseCtx: Parser => Ctx, convert: (Converter[G], Ctx) => Out): (Out, Seq[ExpectedError]) = {
    val lexer = newLexer(stream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser = newParser(tokenStream)

    if(debugOptions.fullContextEnabled)
      parser.getInterpreter.setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION)

    val parseErrors = collectParseErrors(lexer, parser, baseOrigin)
    val expectedErrors = getExpectedErrors(tokenStream, baseOrigin)

    val main = parseCtx(parser)

    parseErrors.errors match {
      case Nil =>
      case errs => throw ParseErrors(errs)
    }

    try {
      (
        convert(newConverter(baseOrigin, expectedErrors, tokenStream), main),
        expectedErrors.map(_._3),
      )
    } catch {
      case m: MatchError =>
        throw ParseMatchError(m.getMessage())
    }
  }

  def parseReaderOrThrow[Ctx, G, Out](reader: Reader, baseOrigin: Origin = Origin(Nil))(parseCtx: Parser => Ctx, convert: (Converter[G], Ctx) => Out): (Out, Seq[ExpectedError]) =
    parseAntlrStreamOrThrow(CharStreams.fromReader(reader), baseOrigin)(parseCtx, convert)

  def parseOrThrow[Ctx, G, Out](readable: Readable, baseOrigin: Origin = Origin(Nil))(parseCtx: Parser => Ctx, convert: (Converter[G], Ctx) => Out): (Out, Seq[ExpectedError]) =
    try {
      readable.read { reader =>
        parseReaderOrThrow(reader, baseOrigin.withContent(ReadableOrigin(readable)))(parseCtx, convert)
      }
    } catch {
      case _: FileNotFoundException | _: NoSuchFileException => throw FileNotFound(readable.fileName)
    }

  def parseAntlrStream[G](stream: runtime.CharStream, baseOrigin: Origin = Origin(Nil)): ParseResult[G] = {
    val (result, expectedErrors) = parseAntlrStreamOrThrow(stream, baseOrigin)(parser => parseMain(parser), mainToResult[G])
    ParseResult(result.asInstanceOf[Seq[GlobalDeclaration[G]]], expectedErrors)
  }

  override def parseReader[G](reader: Reader, baseOrigin: Origin = Origin(Nil)): ParseResult[G] =
    parseAntlrStream(CharStreams.fromReader(reader), baseOrigin)
}
