package vct.parsers

import com.typesafe.scalalogging.LazyLogging
import org.antlr.v4.runtime
import org.antlr.v4.runtime.atn.{
  ATNConfigSet,
  ATNState,
  SingletonPredictionContext,
}
import org.antlr.v4.runtime.dfa.DFA
import org.antlr.v4.runtime.{
  ANTLRErrorListener,
  RecognitionException,
  Recognizer,
  Token,
}
import vct.col.origin.{Origin, PositionRange}
import vct.parsers.debug.{ATNTools, DebugOptions}
import vct.parsers.err.ParseError
import vct.parsers.transform.OriginProvider

import java.util
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.jdk.StreamConverters._

case class CollectingErrorListener(
    baseOrigin: Origin,
    debugOptions: DebugOptions,
) extends ANTLRErrorListener with LazyLogging {
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
        errors :+= ParseError(OriginProvider(baseOrigin, token, token), message)
      case _ =>
        errors :+= ParseError(
          OriginProvider(
            baseOrigin,
            line - 1,
            line - 1,
            Some((charPositionInLine - 1, charPositionInLine - 1)),
          ),
          message,
        )
    }

    currentFullContext = None
  }

  case class FullContext(parser: runtime.Parser, dfa: DFA, startIndex: Int) {
    var startTime: Long = -1
    var stopTime: Long = -1
    var stopIndex: Int = -1
    var ruleIndex: Int = -1
    var decisionState: ATNState = null
    var conflictingBeforeFullContext: util.BitSet = new util.BitSet
    var conflictingAfterFullContext: util.BitSet = conflictingBeforeFullContext

    def origin: Origin = {
      val start = parser.getTokenStream.get(startIndex)
      val stop = parser.getTokenStream.get(stopIndex)
      val startLineIdx = start.getLine - 1
      val startColIdx = start.getCharPositionInLine
      val endLineIdx = stop.getLine - 1
      val endColIdx =
        stop.getCharPositionInLine + stop.getStopIndex - stop.getStartIndex + 1
      baseOrigin.withContent(
        PositionRange(startLineIdx, endLineIdx, Some((startColIdx, endColIdx)))
      )
    }

    def report(): Unit = {
      val time = f"${(stopTime - startTime) / 1_000_000_000.0}%.2f"

      val message = new StringBuilder
      message ++=
        s"Prediction of rule ${parser.getRuleNames()(ruleIndex)} required full context and took ${time}s:\n"

      val ruleState = parser.getATN.ruleToStartState(ruleIndex)

      for ((trans, alt) <- decisionState.getTransitions.zipWithIndex) {
        if (!conflictingBeforeFullContext.get(alt + 1))
          message ++= "    "
        else if (!conflictingAfterFullContext.get(alt + 1))
          message ++= "[x] "
        else
          message ++= " >  "

        val lang = ATNTools.language(parser, trans.target, ruleState.stopState)
        message ++= lang.toString
        message ++= "\n"
      }

      message.setLength(message.length() - 1)

      logger.warn(origin.messageInContext(message.toString()))
    }
  }

  var currentFullContext: Option[FullContext] = None

  override def reportAttemptingFullContext(
      parser: runtime.Parser,
      dfa: DFA,
      startIndex: Int,
      stopIndex: Int,
      conflictingAlts: util.BitSet,
      configs: ATNConfigSet,
  ): Unit = {
    if (!debugOptions.fullContextEnabled)
      return

    currentFullContext = Some(FullContext(parser, dfa, startIndex))

    currentFullContext.get.ruleIndex = parser.getRuleContext.getRuleIndex
    currentFullContext.get.conflictingBeforeFullContext = conflictingAlts
    currentFullContext.get.startTime = System.nanoTime()
  }

  override def reportAmbiguity(
      parser: runtime.Parser,
      dfa: DFA,
      startIndex: Int,
      stopIndex: Int,
      exact: Boolean,
      ambigAlts: util.BitSet,
      configs: ATNConfigSet,
  ): Unit = {
    if (!debugOptions.fullContextEnabled)
      return

    currentFullContext.get.decisionState = dfa.atnStartState
    currentFullContext.get.conflictingAfterFullContext = ambigAlts
    currentFullContext.get.stopIndex = stopIndex
    currentFullContext.get.stopTime = System.nanoTime()

    if (debugOptions.reportAmbiguities)
      currentFullContext.get.report()

    currentFullContext = None
  }

  override def reportContextSensitivity(
      parser: runtime.Parser,
      dfa: DFA,
      startIndex: Int,
      stopIndex: Int,
      prediction: Int,
      configs: ATNConfigSet,
  ): Unit = {
    if (!debugOptions.fullContextEnabled)
      return

    currentFullContext.get.decisionState = dfa.atnStartState
    currentFullContext.get.conflictingAfterFullContext = new util.BitSet()
    currentFullContext.get.conflictingAfterFullContext.set(prediction)
    currentFullContext.get.stopIndex = stopIndex
    currentFullContext.get.stopTime = System.nanoTime()

    if (debugOptions.reportContextSensitivity)
      currentFullContext.get.report()

    currentFullContext = None
  }
}
