package vct.main.stages

import hre.io.Writeable
import hre.stages.Stage
import vct.col.ast.Verification
import vct.col.origin.ExpectedError
import vct.col.rewrite.Generation
import vct.options.{Options, types}
import viper.api.{Carbon, Silicon}

case object Backend {

  def ofOptions(options: Options): Backend = options.backend match {
    case types.Backend.Silicon =>
      val printRawQuantifier = options.siliconPrintQuantifierStats match {
        case Some(freq) => Seq(
          "smt.qi.profile" -> "true",
          "smt.qi.profile_freq" -> s"$freq"
        )
        case None => Seq()
      }
      val z3LogFile = options.devSiliconZ3LogFile match {
        case Some(p) => Seq(
          "trace" -> "true",
          "proof" -> "true",
          "trace-file-name" -> ("\"" + p.toString + "\"")
        )
        case None => Seq()
      }
      val numberOfParallelVerifiers =
        if (options.devSiliconZ3LogFile.isDefined || options.siliconPrintQuantifierStats.isDefined) { Some(1) }
        else { options.devSiliconNumVerifiers }
      SilverBackend(Silicon(
        z3Settings = (printRawQuantifier ++ z3LogFile).toMap,
        z3Path = options.z3Path,
        numberOfParallelVerifiers = numberOfParallelVerifiers,
        proverLogFile = options.devViperProverLogFile,
        printQuantifierStatistics = options.siliconPrintQuantifierStats.isDefined,
        options = options.backendFlags,
      ), options.backendFile)

    case types.Backend.Carbon => SilverBackend(Carbon(
      z3Path = options.z3Path,
      boogiePath = options.boogiePath,
      printFile = options.devViperProverLogFile,
      proverLogFile = options.devCarbonBoogieLogFile,
      options = options.backendFlags,
    ), options.backendFile)
  }
}

trait Backend extends Stage[Verification[_ <: Generation], Seq[ExpectedError]] {
  override def friendlyName: String = "Verification"
  override def progressWeight: Int = 5
}

case class SilverBackend(backend: viper.api.SilverBackend, output: Option[Writeable] = None) extends Backend {
  override def run(input: Verification[_ <: Generation]): Seq[ExpectedError] = {
    input.tasks.foreach { task =>
      backend.submit(task.program, output)
    }
    input.tasks.flatMap(_.expectedErrors)
  }
}