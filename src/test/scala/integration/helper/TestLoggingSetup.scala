package integration.helper

import vct.main.{LoggingSetup, LoggingSetupTrait}

class TestLoggingSetup extends LoggingSetupTrait {
  private val loggingSetup = new LoggingSetup()

  override def setupLoggingBeforeOptions(): Unit = {
    loggingSetup.setupLoggingBeforeOptions()
  }

  override def setupLogging(): Unit = {
    loggingSetup.setupLoggingWithoutForbiddenPrintStream()
  }
}
