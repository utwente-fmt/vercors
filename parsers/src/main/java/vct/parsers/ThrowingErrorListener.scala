package vct.parsers

import hre.lang.System.Warning
import org.antlr.v4.runtime
import org.antlr.v4.runtime.{ANTLRErrorListener, RecognitionException, Recognizer, Token}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import vct.parsers.transform.OriginProvider
import vct.result.VerificationError.Unreachable

import java.util

case class ThrowingErrorListener(originProvider: OriginProvider) extends ANTLRErrorListener {
  override def syntaxError(recognizer: Recognizer[_, _], anyToken: Any,
                           line: Int, charPositionInLine: Int, message: String, e: RecognitionException): Unit = {
    anyToken match {
      case token: Token if anyToken != null =>
        throw ParseError(originProvider(token, token), message)
      case _ =>
        throw Unreachable("ANTLR gave us a null or mistyped token while reporting a syntax error.")
    }
  }

  def report(): Unit = {}

  override def reportAmbiguity(parser: runtime.Parser, dfa: DFA,
                               startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: util.BitSet,
                               configs: ATNConfigSet): Unit = {}

  override def reportAttemptingFullContext(parser: runtime.Parser, dfa: DFA,
                                           startIndex: Int, stopIndex: Int, conflictingAlts: util.BitSet,
                                           configs: ATNConfigSet): Unit = {}

  override def reportContextSensitivity(parser: runtime.Parser, dfa: DFA,
                                        startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet): Unit = {}
}
