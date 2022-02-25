package vct.parsers

import hre.lang.System.Failure
import org.antlr.v4.runtime
import org.antlr.v4.runtime.{CommonTokenStream, Token}
import vct.col.util.ExpectedError
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

  def expectedErrors(lexer: CommonTokenStream, channel: Int,
                     startToken: Int, endToken: Int,
                     originProvider: OriginProvider, blameProvider: BlameProvider): Seq[(Token, Token, ExpectedError)] = {
    lexer.fill()
    var startStack: Seq[(String, Token)] = Nil
    var errors = Seq.empty[(Token, Token, ExpectedError)]

    for(token <- lexer.getTokens.asScala.filter(_.getChannel == channel)) {
      if(token.getType == startToken) {
        val code = token.getText.replace("/*", "").replace("*/", "").replace("[/expect", "").replace("]", "").strip()
        startStack :+= (code, token)
      }

      if(token.getType == endToken) {
        if(startStack.isEmpty) throw UnbalancedExpectedError(token)
        val (code, start) = startStack.last
        startStack = startStack.init
        val err = ExpectedError(code, originProvider(start, token), blameProvider(start, token))
        errors :+= (start, token, err)
      }
    }

    errors.sortBy(_._1.getTokenIndex)
  }

  def parse[G](stream: runtime.CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G]

  def parse[G](stream: InputStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] =
    parse(runtime.CharStreams.fromStream(stream), originProvider, blameProvider)

  def parse[G](f: File)(originProvider: OriginProvider = FileOriginProvider(f.toPath),
                     blameProvider: BlameProvider = FileOriginProvider(f.toPath)): ParseResult[G] = {
    val name = f.toString
    try {
      parse(new FileInputStream(f), originProvider, blameProvider)
    } catch {
      case _: FileNotFoundException =>
        throw FileNotFound(f.toPath)
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