package vct.main.stages

import hre.io.Writeable
import vct.col.ast.Program
import vct.col.rewrite.Generation
import vct.col.util.ExpectedError
import vct.options.Options
import viper.api.{Carbon, Silicon}

case object Backend {

  def ofOptions(options: Options): Backend = options.backend match {
    case vct.options.Backend.Silicon =>
      val printRawQuantifier = options.devSiliconPrintRawQuantifierStats match {
        case Some(amount) => Seq(
          "smt.qi.profile" -> "true",
          "smt.qi.profile_freq" -> s"$amount"
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
        if (options.devSiliconZ3LogFile.isDefined) { Some(1) }
        else { options.devSiliconNumVerifiers }
      SilverBackend(Silicon(
        z3Settings = (printRawQuantifier ++ z3LogFile).toMap,
        z3Path = options.z3Path,
        numberOfParallelVerifiers = numberOfParallelVerifiers,
        logLevel = if (options.devSiliconPrintRawQuantifierStats.isDefined) { Some("INFO") } else { None },
        proverLogFile = options.devViperProverLogFile,
      ), options.backendFile)

    case vct.options.Backend.Carbon => SilverBackend(Carbon(
      z3Path = options.z3Path,
      boogiePath = options.boogiePath,
      printFile = options.devViperProverLogFile,
      proverLogFile = options.devCarbonBoogieLogFile,
    ), options.backendFile)
  }
}

trait Backend extends ContextStage[Program[_ <: Generation], Seq[ExpectedError], Unit] {
  override def friendlyName: String = "Verification"
  override def progressWeight: Int = 5
}

case class SilverBackend(backend: viper.api.SilverBackend, output: Option[Writeable] = None) extends Backend {
  override def runWithoutContext(input: Program[_ <: Generation]): Unit = {
    backend.submit(input, output)
  }
}