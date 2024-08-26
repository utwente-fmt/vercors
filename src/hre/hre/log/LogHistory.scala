package hre.log

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import ch.qos.logback.core.encoder.Encoder
import hre.log.LogHistory.MAX_LENGTH

import java.nio.charset.StandardCharsets

object LogHistory {
  val messages = new StringBuffer
  val MAX_LENGTH = 1_000_000
}

class LogHistory extends AppenderBase[ILoggingEvent] {
  override def append(e: ILoggingEvent): Unit = {
    if (!e.getMessage.isBlank && LogHistory.messages.length() < MAX_LENGTH)
      LogHistory.messages
        .append(new String(enc.encode(e), StandardCharsets.UTF_8))
  }

  var enc: Encoder[ILoggingEvent] = null

  def setEncoder(encoder: Encoder[ILoggingEvent]): Unit = enc = encoder
}
