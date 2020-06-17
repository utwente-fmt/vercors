package vct.travis

import scala.collection.JavaConverters._

import hre.config.{OptionParser, StringListSetting}
import hre.lang.System.{Abort, Output}
import vct.test.Jacoco

object CoverageAggregator {
  def main(args: Array[String]): Unit = {
    hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Debug)
    hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Debug)

    val clops = new OptionParser
    clops.add(clops.getHelpOption, "help")

    val mergeReports = new StringListSetting
    clops.add(mergeReports.getAppendOption("Indicates reports to be merged (comma separated)"), "merge")

    val input: Array[String] = clops.parse(args)
    if (input.length != 0) {
      Abort("Unmatched command line args: %s", input.toSeq.mkString(" "))
    }

    val mergeReportPaths = mergeReports.iterator().asScala.toSeq
    if (mergeReportPaths.length == 0) {
      Abort("No merge reports given")
    }

    Jacoco.aggregateExecFiles(mergeReportPaths, "jacoco.exec")
  }
}
