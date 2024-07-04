package vct.main.modes

import com.typesafe.scalalogging.LazyLogging

import vct.col.origin.BlameCollector
import vct.main.stages.Stages
import vct.options.Options
import vct.options.types.PathOrStd
import vct.parsers.transform.ConstantBlameProvider

object CSimplifier extends LazyLogging {
  def verifyWithOptions(options: Options, inputs: Seq[PathOrStd]) = {
    val collector = BlameCollector()
    val stages = Stages
      .cSimplificationWithOptions(options, ConstantBlameProvider(collector))
    stages.run(inputs) match {
      case Left(value) => logger.error(value.text)
      case Right(()) => logger.info("Runtime terminated successfully.")
    }
  }

  def runOptions(options: Options): Int = {
    verifyWithOptions(options, options.inputs)
    0
  }

}
