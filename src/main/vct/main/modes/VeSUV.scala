package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import vct.col.origin.BlameCollector
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS}
import vct.main.stages.Stages
import vct.options.Options
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError.{SystemError, UserError}

case object VeSUV extends LazyLogging {
  def runOptions(options: Options): Int = {
    val collector = BlameCollector()
    val stages = Stages
      .vesuvOfOptions(options, ConstantBlameProvider(collector))
    stages.run(options.inputs) match {
      case Left(_: UserError) => EXIT_CODE_ERROR
      case Left(err: SystemError) => throw err
      case Right(()) =>
        logger.info("Transformation complete")
        EXIT_CODE_SUCCESS
    }
  }
}
