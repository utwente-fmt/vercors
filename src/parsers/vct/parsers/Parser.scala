package vct.parsers

import hre.io.Readable
import org.antlr.v4.runtime
import org.antlr.v4.runtime.{
  BailErrorStrategy,
  CharStreams,
  CommonTokenStream,
  Token,
}
import vct.col.origin.ExpectedError
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.result.VerificationError.UserError

import java.io.FileNotFoundException
import scala.jdk.CollectionConverters._

abstract class Parser(
    val originProvider: OriginProvider,
    val blameProvider: BlameProvider,
) {
  case class UnbalancedExpectedError(tok: Token) extends UserError {
    override def code: String = "unbalancedExpectedError"
    override def text: String = "There is no scope to close here."
  }

  def expectedErrors(
      lexer: CommonTokenStream,
      channel: Int,
      startToken: Int,
      endToken: Int,
  ): Seq[(Token, Token, ExpectedError)] = {
    lexer.fill()
    var startStack: Seq[(String, Token)] = Nil
    var errors = Seq.empty[(Token, Token, ExpectedError)]

    for (token <- lexer.getTokens.asScala.filter(_.getChannel == channel)) {
      if (token.getType == startToken) {
        val code = token.getText.replace("/*", "").replace("*/", "")
          .replace("[/expect", "").replace("]", "").strip()
        startStack :+= (code, token)
      }

      if (token.getType == endToken) {
        if (startStack.isEmpty)
          throw UnbalancedExpectedError(token)
        val (code, start) = startStack.last
        startStack = startStack.init
        val err = ExpectedError(
          code,
          originProvider(start, token),
          blameProvider(start, token),
        )
        errors :+= (start, token, err)
      }
    }

    errors.sortBy(_._1.getTokenIndex)
  }

  protected def getErrorsFor[T](
      parser: runtime.Parser,
      lexer: runtime.Lexer,
      originProvider: OriginProvider,
  )(f: => T): Either[Seq[ParseError], T] = {
    parser.setErrorHandler(ParseErrorStrategy())
    parser.removeErrorListeners()
    lexer.removeErrorListeners()
    val ec = CollectingErrorListener(originProvider)
    parser.addErrorListener(ec)
    lexer.addErrorListener(ec)

    val result = f

    ec.errors match {
      case Nil => Right(result)
      case some => Left(some)
    }
  }

  protected def noErrorsOrThrow[T](
      parser: runtime.Parser,
      lexer: runtime.Lexer,
      originProvider: OriginProvider,
  )(f: => T): T =
    getErrorsFor(parser, lexer, originProvider)(f) match {
      case Left(errs) => throw ParseErrors(errs)
      case Right(t) => t
    }

  def parse[G](stream: runtime.CharStream): ParseResult[G]

  def parse[G](readable: Readable): ParseResult[G] =
    try {
      readable.read { reader =>
        parse(CharStreams.fromReader(reader, readable.fileName))
      }
    } catch {
      case _: FileNotFoundException => throw FileNotFound(readable.fileName)
    }
}
