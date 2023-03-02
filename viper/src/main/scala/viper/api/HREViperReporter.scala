package viper.api

import hre.lang.System.{DebugException, Output}
import viper.silver.reporter._

case class HREViperReporter(name: String = "hre_reporter", timeInfo: Boolean = true) extends Reporter {
  // Code below adapted from viper.silver.reporter.StdIOReporter
  // includes the unit name (e.g., seconds, sec, or s).
  private def timeStr: Time => String = format.formatMillisReadably

  private def plurals(num_things: Int): String = if (num_things==1) "" else "s"

  private def bulletFmt(num_items: Int): String = s"%${num_items.toString.length}d"

  def report(msg: Message): Unit = {
    msg match {
      case OverallFailureMessage(v, t, res) =>
        val num_errors = res.errors.length
        if (!timeInfo)
          Output( s"$v found $num_errors error${plurals(num_errors)}:" )
        else
          Output( s"$v found $num_errors error${plurals(num_errors)} in ${timeStr(t)}:" )
        res.errors.zipWithIndex.foreach { case(e, n) => Output( s"  [${bulletFmt(num_errors).format(n)}] ${e.readableMessage}" ) }

      case OverallSuccessMessage(v, t) =>
        if (!timeInfo)
          Output( s"$v finished verification successfully." )
        else
          Output( s"$v finished verification successfully in ${timeStr(t)}." )

      case ExceptionReport(e) =>
        /** Theoretically, we may encounter an exceptional message that has
          * not yet been reported via AbortedExceptionally. */
        Output( s"Verification aborted exceptionally: ${e.toString}" )
        Option(e.getCause) match {
          case Some(cause) =>
            Output( s"  Cause: ${cause.toString}" )
            DebugException(cause)
          case None =>
        }
      //Output( s"  See log file for more details: ..." )

      case ExternalDependenciesReport(deps) =>
        val s: String = (deps map (dep => {
          s"  ${dep.name} ${dep.version}, located at ${dep.location}."
        })).mkString("\n")
        Output( s"The following dependencies are used:\n$s" )

      case InvalidArgumentsReport(_, errors) =>
        errors.foreach(e => Output(s"  ${e.readableMessage}"))
        Output( s"Run with just --help for usage and options" )

      case CopyrightReport(text) =>
        Output( text )

      case EntitySuccessMessage(_, _, _, _) =>    // FIXME Currently, we only print overall verification results to STDOUT.
      case EntityFailureMessage(_, _, _, _, _) => // FIXME Currently, we only print overall verification results to STDOUT.
      case ConfigurationConfirmation(_) =>     // TODO  use for progress reporting
        //Output( s"Configuration confirmation: $text" )
      case InternalWarningMessage(_) =>        // TODO  use for progress reporting
        //Output( s"Internal warning: $text" )
      case _:SimpleMessage =>
        //Output( sm.text )
      case _ =>
        //Output( s"Cannot properly print message of unsupported type: $msg" )
    }
  }
}
