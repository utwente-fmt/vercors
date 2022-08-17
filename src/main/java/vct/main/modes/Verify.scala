package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import vct.options.Options
import hre.io.Readable
import vct.col.origin.{BlameCollector, TableEntry, VerificationFailure}
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS, EXIT_CODE_VERIFICATION_FAILURE}
import vct.main.stages.Stages
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError

case object Verify extends LazyLogging {
  def verifyWithSilicon(inputs: Seq[Readable]): Either[VerificationError, Seq[VerificationFailure]] = {
    val collector = BlameCollector()
    val stages = Stages.silicon(ConstantBlameProvider(collector))
    logger.debug(stages.toString)
    stages.run(inputs) match {
      case Left(error) => Left(error)
      case Right(()) => Right(collector.errs.toSeq)
    }
  }

  def verifyWithCarbon(inputs: Seq[Readable]): Either[VerificationError, Seq[VerificationFailure]] = {
    val collector = BlameCollector()
    val stages = Stages.carbon(ConstantBlameProvider(collector))
    logger.debug(stages.toString)
    stages.run(inputs) match {
      case Left(error) => Left(error)
      case Right(()) => Right(collector.errs.toSeq)
    }
  }

  def verifyWithOptions(options: Options, inputs: Seq[Readable]): Either[VerificationError, Seq[VerificationFailure]] = {
    val collector = BlameCollector()
    val stages = Stages.ofOptions(options, ConstantBlameProvider(collector))
    logger.debug("Stages: " ++ stages.flatNames.map(_._1).mkString(", "))
    stages.run(inputs) match {
      case Left(error) => Left(error)
      case Right(()) => Right(collector.errs.toSeq)
    }
  }

  def runOptions(options: Options): Int = {
    verifyWithOptions(options, options.inputs) match {
      case Left(err) =>
        logger.error(err.text)
        EXIT_CODE_ERROR
      case Right(Nil) =>
        logger.info("Verification completed successfully.")
        EXIT_CODE_SUCCESS
      case Right(fails) =>
        if(fails.size <= 2) fails.foreach(fail => logger.error(fail.desc))
        else logger.error(TableEntry.render(fails.map(_.asTableEntry)))
        EXIT_CODE_VERIFICATION_FAILURE
    }
  }
}
