package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import vct.col.origin.BlameCollector
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS}
import vct.main.stages.Stages
import vct.options.Options
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError

object Alpinist extends LazyLogging {
  def runOptions(options: Options): Int = {
    val collector = BlameCollector()

    val stages = Stages
      .alpinistOfOptions(options, ConstantBlameProvider(collector))

    stages.run(options.inputs) match {
      case Left(err: VerificationError.UserError) =>
        logger.error(err.text)
        EXIT_CODE_ERROR
      case Left(err: VerificationError.SystemError) => throw err
      case Right(()) => EXIT_CODE_SUCCESS
    }
  }
}
