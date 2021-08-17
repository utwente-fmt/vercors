package vct.main.passes

import hre.config.Configuration
import hre.lang.System.Fail
import vct.logging.PassReport
import vct.main.options.CommandLineOptions
import vct.main.passes.Passes.BY_KEY

import scala.jdk.CollectionConverters.IterableHasAsScala

trait PassesGeneratorTrait {
  def getPasses(report: PassReport): Seq[AbstractPass]
}

class PassesGenerator extends PassesGeneratorTrait {

  def getPasses(report: PassReport): Seq[AbstractPass] = {
    val silverPassesGenerator = new SilverPassesGenerator()
    val veymontPassesGenerator = new VeymontPassesGenerator()

    if (CommandLineOptions.passListOption.used) {
      CommandLineOptions.passList.asScala.map(key => BY_KEY.get(key) match {
        case None => Fail("Unknown pass: %s", key); ???
        case Some(pass) => pass
      }).toSeq
    }else if (CommandLineOptions.silver.used) {
      silverPassesGenerator.getPasses(report)
    }else if (Configuration.veymont_file.used()) {
      veymontPassesGenerator.getPasses(report)
    } else {
      Fail("no back-end or passes specified"); ???
    }
  }

}
