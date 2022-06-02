package vct.main.stages

import hre.io.Writeable
import vct.col.ast.Program
import vct.col.rewrite.Generation
import vct.col.util.ExpectedError
import vct.options.Options
import viper.api.{Carbon, Silicon}

case object Backend {

  def ofOptions(options: Options): Backend = options.backend match {
    case vct.options.Backend.Silicon => SilverBackend(Silicon(
      z3Settings = options.devPrintRawQuantifierStats match {
        case Some(amount) => Map(
          "smt.qi.profile" -> "true",
          "smt.qi.profile_freq" -> (amount + "")
        )
        case None => Map.empty
      },
      z3Path = options.z3Path,
      // In the PR they set the threads to 0, so we also do it here (https://github.com/viperproject/silicon/pull/587)
      numberOfParallelVerifiers = if (options.devPrintRawQuantifierStats.isDefined) { Some(1) } else { None },
      logLevel = if (options.devPrintRawQuantifierStats.isDefined) { Some("INFO") } else { None },
    ), options.backendFile)
    case vct.options.Backend.Carbon => SilverBackend(Carbon(
      z3Path = options.z3Path,
      boogiePath = options.boogiePath,
    ), options.backendFile)
  }
}

trait Backend extends ContextStage[Program[_ <: Generation], Seq[ExpectedError], Unit] {
  override def friendlyName: String = "Verification"
  override def progressWeight: Int = 5
}

case class SilverBackend(backend: viper.api.SilverBackend, output: Option[Writeable] = None) extends Backend {
  override def runWithoutContext(input: Program[_ <: Generation]): Unit =
    backend.submit(input, output)
}