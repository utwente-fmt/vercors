package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import vct.col.origin.BlameCollector
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS, EXIT_CODE_VERIFICATION_FAILURE}
import vct.main.modes.Verify.logger
import vct.main.stages.Stages
import vct.options.Options
import vct.options.types.PathOrStd
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError.{SystemError, UserError}
import viper.carbon.boogie.Implicits.lift

object VeyMont extends LazyLogging {

  def verifyWithOptions(options: Options, inputs: Seq[PathOrStd]) = {
    val collector = BlameCollector()
    val stages = Stages.veymontTransformationOfOptions(options, ConstantBlameProvider(collector))
    logger.debug("Stages: " ++ stages.flatNames.map(_._1).mkString(", "))
    stages.run(inputs) match {
      case Left(err: UserError) => logger.error(err.text)
      case Left(err: SystemError) => throw err
      case Right(()) => logger.info("VeyMont terminated successfully.")
    }

  }

  def runOptions(options: Options): Int = {
    verifyWithOptions(options, options.inputs)
    0
  }

}
