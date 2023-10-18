package vct.parsers

import com.typesafe.scalalogging.LazyLogging
import org.antlr.v4.runtime
import org.antlr.v4.runtime.{
  ANTLRErrorListener,
  RecognitionException,
  Recognizer,
  Token,
}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import vct.parsers.transform.OriginProvider
import vct.result.VerificationError.Unreachable

import java.util

case class CollectingErrorListener(originProvider: OriginProvider)
    extends ANTLRErrorListener with LazyLogging {
  var errors: Seq[ParseError] = Nil

  override def syntaxError(
      recognizer: Recognizer[_, _],
      anyToken: Any,
      line: Int,
      charPositionInLine: Int,
      message: String,
      e: RecognitionException,
  ): Unit = {
    anyToken match {
      case token: Token if anyToken != null =>
        errors :+= ParseError(originProvider(token, token), message)
      case _ =>
        errors :+= ParseError(
          originProvider(
            line - 1,
            line - 1,
            Some((charPositionInLine - 1, charPositionInLine - 1)),
          ),
          message,
        )
    }
  }

  def report(): Unit = {}

  override def reportAmbiguity(
      parser: runtime.Parser,
      dfa: DFA,
      startIndex: Int,
      stopIndex: Int,
      exact: Boolean,
      ambigAlts: util.BitSet,
      configs: ATNConfigSet,
  ): Unit = {}

  override def reportAttemptingFullContext(
      parser: runtime.Parser,
      dfa: DFA,
      startIndex: Int,
      stopIndex: Int,
      conflictingAlts: util.BitSet,
      configs: ATNConfigSet,
  ): Unit = {}

  override def reportContextSensitivity(
      parser: runtime.Parser,
      dfa: DFA,
      startIndex: Int,
      stopIndex: Int,
      prediction: Int,
      configs: ATNConfigSet,
  ): Unit = {}
}
