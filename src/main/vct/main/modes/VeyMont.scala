package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import hre.io.CollectString
import hre.stages.Stage
import hre.util.Time.logTime
import vct.col.origin.{BlameCollector, VerificationFailure}
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS}
import vct.main.stages.Stages
import vct.options.Options
import vct.result.VerificationError
import vct.result.VerificationError.UserError

object VeyMont extends LazyLogging {
  case class ChoreographyVerificationError(failures: Seq[VerificationFailure])
      extends UserError {
    override def text: String = {
      val fails = failures.map(_.desc).mkString("\n")
      s"Verification of the input choreography failed because of the following failures:\n$fails"
    }

    override def code: String = "veymont:choreographyVerificationFailed"
  }

  case class ImplementationVerificationError(failures: Seq[VerificationFailure])
      extends UserError {
    override def text: String = {
      val fails = failures.map(_.desc).mkString("\n")
      s"Verification of the generated implementation failed because of the following failuers:\n$fails"
    }

    override def code: String = "veymont:implementationVerificationFailed"
  }

  case class NoVerificationFailures(
      collector: BlameCollector,
      error: Seq[VerificationFailure] => UserError,
  ) extends Stage[Unit, Unit] {
    override def friendlyName: String = "noVerificationErrors"
    override def progressWeight: Int = 1
    override def run(in: Unit): Unit =
      if (collector.errs.nonEmpty) { throw error(collector.errs.toSeq) }
  }

  def runOptions(options: Options): Int =
    logTime(
      "VeyMont mode", {
        val stages = Stages.veymontOfOptions(options)
        stages.run(options.inputs) match {
          case Left(err: VerificationError.UserError) =>
            logger.error(err.text)
            EXIT_CODE_ERROR
          case Left(err: VerificationError.SystemError) =>
            logger.error(CollectString(s => err.printStackTrace(s)))
            EXIT_CODE_ERROR
          case Right(()) =>
            logger.info("VeyMont success")
            EXIT_CODE_SUCCESS
        }
      },
    )

}
