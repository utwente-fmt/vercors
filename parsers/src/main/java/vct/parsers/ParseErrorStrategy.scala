package vct.parsers

import org.antlr.v4.runtime
import org.antlr.v4.runtime.DefaultErrorStrategy

case class ParseErrorStrategy() extends DefaultErrorStrategy {
  override def reportMissingToken(recognizer: runtime.Parser): Unit = {
    super.reportMissingToken(recognizer)
  }
}
