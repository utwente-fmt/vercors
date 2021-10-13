package vct.parsers

import hre.lang.System.Failure
import org.antlr.v4.runtime
import org.antlr.v4.runtime.{CommonTokenStream, Token}
import vct.parsers.transform.{BlameProvider, FileOriginProvider, OriginProvider}
import vct.result.VerificationResult.UserError

import java.io.{File, FileInputStream, FileNotFoundException, InputStream}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

abstract class Parser {
  case class UnbalancedExpectedError(tok: Token) extends UserError {
    override def code: String = "unbalancedExpectedError"
    override def text: String = "There is no scope to close here."
  }

  def expectedErrors(lexer: CommonTokenStream, channel: Int, startToken: Int, endToken: Int): mutable.Map[(Int, Int), String] = {
    lexer.fill()
    var startStack: Seq[(String, Int)] = Nil
    val errors = mutable.Map[(Int, Int), String]()

    for(token <- lexer.getTokens.asScala.filter(_.getChannel == channel)) {
      if(token.getType == startToken) {
        val code = token.getText.substring(9, token.getText.length-1)
        startStack :+= (code, token.getTokenIndex)
      }

      if(token.getType == endToken) {
        if(startStack.isEmpty) throw UnbalancedExpectedError(token)
        val (code, startIndex) = startStack.last
        startStack = startStack.init
        errors += (startIndex, token.getTokenIndex) -> code
      }
    }

    errors
  }

  def parse(stream: runtime.CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult

  def parse(stream: InputStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult =
    parse(runtime.CharStreams.fromStream(stream), originProvider, blameProvider)

  def parse(f: File)(originProvider: OriginProvider = FileOriginProvider(f.toPath),
                     blameProvider: BlameProvider = FileOriginProvider(f.toPath)): ParseResult = {
    val name = f.toString
    try {
      parse(new FileInputStream(f), originProvider, blameProvider)
    } catch {
      case _: FileNotFoundException =>
        throw Failure("Could not find file: %s", name)
    }
  }

  protected def errorCounter(parser: runtime.Parser, lexer: runtime.Lexer, originProvider: OriginProvider): ThrowingErrorListener = {
    parser.removeErrorListeners()
    lexer.removeErrorListeners()
    val ec = ThrowingErrorListener(originProvider)
    parser.addErrorListener(ec)
    lexer.addErrorListener(ec)
    ec
  }
}