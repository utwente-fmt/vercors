package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import vct.col.origin.BlameCollector
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS}
import vct.main.stages.Stages
import vct.options.Options
import vct.parsers.transform.ConstantBlameProvider

case object Transform extends LazyLogging {
  def runOptions(options: Options): Int = {
    val collector = BlameCollector()
    val stages = Stages
      .vesuvOfOptions(options, ConstantBlameProvider(collector))
    stages.run(options.inputs) match {
      case Left(_) => EXIT_CODE_ERROR
      case Right(()) =>
        logger.info("Transformation complete")
        EXIT_CODE_SUCCESS
    }
  }
}
