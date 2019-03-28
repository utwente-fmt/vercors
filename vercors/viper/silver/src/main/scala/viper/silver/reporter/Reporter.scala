/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.reporter

trait Reporter {
  val name: String

  def report(msg: Message): Unit
}

object NoopReporter extends Reporter {
  val name: String = "NoopReporter"
  def report(msg: Message): Unit = ()
}

case class StdIOReporter(name: String = "stdout_reporter", timeInfo: Boolean = true) extends Reporter {
  
  var counter = 0

  // includes the unit name (e.g., seconds, sec, or s).
  private def timeStr: Time => String = format.formatMillisReadably

  private def plurals(num_things: Int): String = if (num_things==1) "" else "s"

  private def bulletFmt(num_items: Int): String = s"%${num_items.toString.length}d"

  def report(msg: Message): Unit = {
    msg match {
      case OverallFailureMessage(v, t, res) =>
        val num_errors = res.errors.length
        if (!timeInfo)
          println( s"$v found $num_errors error${plurals(num_errors)}:" )
        else
          println( s"$v found $num_errors error${plurals(num_errors)} in ${timeStr(t)}:" )
        res.errors.zipWithIndex.foreach { case(e, n) => println( s"  [${bulletFmt(num_errors).format(n)}] ${e.readableMessage}" ) }

      case OverallSuccessMessage(v, t) =>
        if (!timeInfo)
          println( s"$v finished verification successfully." )
        else
          println( s"$v finished verification successfully in ${timeStr(t)}." )

      case ExceptionReport(e) =>
        /** Theoretically, we may encounter an exceptional message that has
          * not yet been reported via AbortedExceptionally. */
        println( s"Verification aborted exceptionally: ${e.toString}" )
        Option(e.getCause) match {
          case Some(cause) => println( s"  Cause: ${cause.toString}" )
          case None =>
        }
        //println( s"  See log file for more details: ..." )

      case ExternalDependenciesReport(deps) =>
        val s: String = (deps map (dep => {
          s"  ${dep.name} ${dep.version}, located at ${dep.location}."
        })).mkString("\n")
        println( s"The following dependencies are used:\n$s" )

      case InvalidArgumentsReport(tool_sig, errors) =>
        errors.foreach(e => println(s"  ${e.readableMessage}"))
        println( s"Run with just --help for usage and options" )

      case CopyrightReport(text) =>
        println( text )

      case EntitySuccessMessage(_, _, _) =>    // FIXME Currently, we only print overall verification results to STDOUT.
      case EntityFailureMessage(_, _, _, _) => // FIXME Currently, we only print overall verification results to STDOUT.
      case ConfigurationConfirmation(_) =>     // TODO  use for progress reporting
        //println( s"Configuration confirmation: $text" )
      case InternalWarningMessage(_) =>        // TODO  use for progress reporting
        //println( s"Internal warning: $text" )
      case sm:SimpleMessage =>
        //println( sm.text )
      case _ =>
        //println( s"Cannot properly print message of unsupported type: $msg" )
    }
    counter = counter + 1
  }
}

