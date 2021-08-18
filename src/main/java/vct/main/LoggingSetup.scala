package vct.main

import hre.io.ForbiddenPrintStream
import vct.main.options.CommandLineOptions

import scala.jdk.CollectionConverters.MapHasAsScala

trait LoggingSetupTrait {
  def setupLoggingBeforeOptions() : Unit
  def setupLogging(): Unit
}
class LoggingSetup extends LoggingSetupTrait {

  def setupLoggingBeforeOptions(): Unit = {
    hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info)
    hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info)
  }

  def setupLoggingWithoutForbiddenPrintStream(): Unit ={
    import hre.lang.System.LogLevel

    var level = CommandLineOptions.logLevel.get match {
      case "silent" => LogLevel.Silent
      case "abort" => LogLevel.Abort
      case "result" => LogLevel.Result
      case "warning" => LogLevel.Warning
      case "info" => LogLevel.Info
      case "progress" => LogLevel.Progress
      case "debug" => LogLevel.Debug
      case "all" => LogLevel.All
    }

    if (!CommandLineOptions.debugFilters.get.isEmpty && level.getOrder < hre.lang.System.LogLevel.Debug.getOrder)
      level = hre.lang.System.LogLevel.Debug

    for (filter <- CommandLineOptions.debugFilters.get.asScala.keys) {
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
