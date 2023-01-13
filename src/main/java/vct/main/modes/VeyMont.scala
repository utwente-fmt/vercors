package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import vct.col.origin.BlameCollector
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS, EXIT_CODE_VERIFICATION_FAILURE}
import vct.main.modes.Verify.logger
import vct.main.stages.Stages
import vct.options.Options
import vct.options.types.PathOrStd
import vct.parsers.transform.ConstantBlameProvider
import viper.carbon.boogie.Implicits.lift

object VeyMont extends LazyLogging {

  def verifyWithOptions(options: Options, inputs: Seq[PathOrStd]) = {
    val collector = BlameCollector()
    val stages = Stages.veymontOfOptions(options, ConstantBlameProvider(collector))
    logger.debug("Stages: " ++ stages.flatNames.map(_._1).mkString(", "))
    stages.run(inputs)

  }

  def runOptions(options: Options): Int = {
    verifyWithOptions(options, options.inputs)
    0
  }

}
