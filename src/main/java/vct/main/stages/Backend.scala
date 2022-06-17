package vct.main.stages

import hre.io.Writeable
import vct.col.ast.{Program, Verification}
import vct.col.rewrite.Generation
import vct.col.util.ExpectedError
import vct.options.Options
import viper.api.{Carbon, Silicon}

case object Backend {
  def ofOptions(options: Options): Backend = options.backend match {
    case vct.options.Backend.Silicon => SilverBackend(Silicon(
      z3Settings = Map.empty,
      z3Path = options.z3Path,
      options = options.backendFlags,
    ), options.backendFile)
    case vct.options.Backend.Carbon => SilverBackend(Carbon(
      z3Path = options.z3Path,
      boogiePath = options.boogiePath,
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