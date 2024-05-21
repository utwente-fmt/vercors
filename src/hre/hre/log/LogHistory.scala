package hre.log

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import ch.qos.logback.core.encoder.Encoder

import java.nio.charset.StandardCharsets

object LogHistory {
  val messages = new StringBuffer
}

class LogHistory extends AppenderBase[ILoggingEvent] {
  override def append(e: ILoggingEvent): Unit = {
    if(!e.getMessage.isBlank)
      LogHistory.messages.append(new String(enc.encode(e), StandardCharsets.UTF_8))
  }

  var enc: Encoder[ILoggingEvent] = null

  def setEncoder(encoder: Encoder[ILoggingEvent]): Unit =
    enc = encoder
}
