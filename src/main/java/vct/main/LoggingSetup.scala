package vct.main

import hre.config.Configuration
import hre.io.ForbiddenPrintStream
import hre.lang
import hre.lang.LogLevel

import scala.jdk.CollectionConverters.MapHasAsScala

trait LoggingSetupTrait {
  def setupLoggingBeforeOptions() : Unit
  def setupLogging(): Unit
}
class LoggingSetup extends LoggingSetupTrait {

  def setupLoggingBeforeOptions(): Unit = {
    hre.lang.System.setOutputStream(System.out, LogLevel.Info)
    hre.lang.System.setErrorStream(System.err, lang.LogLevel.Info)
  }

  def setupLoggingWithoutForbiddenPrintStream(): Unit ={
    import hre.lang.LogLevel

    var level = Configuration.currentConfiguration.logLevel.get match {
      case "silent" => LogLevel.Silent
      case "abort" => LogLevel.Abort
      case "result" => LogLevel.Result
      case "warning" => LogLevel.Warning
      case "info" => LogLevel.Info
      case "progress" => LogLevel.Progress
      case "debug" => LogLevel.Debug
      case "all" => LogLevel.All
    }

    if (!Configuration.currentConfiguration.debugFilters.get.isEmpty && level.getOrder < lang.LogLevel.Debug.getOrder)
      level = lang.LogLevel.Debug

    for (filter <- Configuration.currentConfiguration.debugFilters.get.asScala.keys) {
      if (filter.contains(":") /* With line number */ ) hre.lang.System.addDebugFilterByLine(filter)
      else hre.lang.System.addDebugFilterByClassName(filter)
    }

    hre.lang.System.setOutputStream(System.out, level)
    hre.lang.System.setErrorStream(System.err, level)
  }

  def setupLogging(): Unit = {
    setupLoggingWithoutForbiddenPrintStream()
    System.setErr(new ForbiddenPrintStream(System.err))
    System.setOut(new ForbiddenPrintStream(System.out))
  }

}
