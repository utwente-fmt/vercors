package hre.progress

import ch.qos.logback.classic.pattern.ThrowableProxyConverter
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.{CoreConstants, LayoutBase}

class ProgressAwareLogbackLayout extends LayoutBase[ILoggingEvent] {
  private val tpc = new ThrowableProxyConverter()

  override def start(): Unit = {
    super.start()
    tpc.start()
  }

  override def doLayout(event: ILoggingEvent): String =
    Progress.undoProgressMessage +
      (if(event.getFormattedMessage.contains('\n')) {
        event.getFormattedMessage.strip +
          CoreConstants.LINE_SEPARATOR
      } else {
        s"[${event.getLevel}] " +
          event.getFormattedMessage +
          CoreConstants.LINE_SEPARATOR
      }) +
      tpc.convert(event) +
      Progress.progressMessage
}
