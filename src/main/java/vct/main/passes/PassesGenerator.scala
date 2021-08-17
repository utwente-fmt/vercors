package vct.main.passes

import hre.config.Configuration
import hre.lang.System.Fail
import vct.main.passes.Passes.BY_KEY

trait PassesGeneratorTrait {
  def getPasses: Seq[AbstractPass]
}

class PassesGenerator extends PassesGeneratorTrait {

  def getPasses: Seq[AbstractPass] = {
    if (pass_list_option.used) {
      pass_list.asScala.map(key => BY_KEY.get(key) match {
        case None => Fail("Unknown pass: %s", key); ???
        case Some(pass) => pass
      }).toSeq
    }
    else if (silver.used) collectPassesForSilver
    else if (Configuration.veymont_file.used()) collectPassesForVeyMont
    else { Fail("no back-end or passes specified"); ??? }
  }

}
