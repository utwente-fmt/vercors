package vct.main.stages

import hre.io.Readable
import hre.stages.{SaveInputStage, Stage, Stages}
import vct.col.ast.Verification
import vct.col.rewrite.Generation
import vct.options.Options
import vct.parsers.transform.BlameProvider

object AlpinistVerification {
  def ofOptions(
      options: Options,
      blameProvider: BlameProvider,
  ): Stage[Seq[Readable], Verification[_ <: Generation]] =
    AlpinistVerification(
      Parsing.ofOptions(options, blameProvider)
        .thenRun(Resolution.ofOptions(options, blameProvider)),
      Transformation.ofOptions(options).thenRun(Backend.ofOptions(options))
        .thenRun(ExpectedErrors.ofOptions(options)),
    )
}

case class AlpinistVerification(
    parse: Stages[Seq[Readable], Verification[_ <: Generation]],
    verify: Stages[Verification[_ <: Generation], Unit],
) extends Stage[Seq[Readable], Verification[_ <: Generation]] {
  override def friendlyName: String = "Verification"
  override def progressWeight: Int = 1

  override def run(in: Seq[Readable]): Verification[_ <: Generation] = {
    parse.thenRun(SaveInputStage(verify)).transform(_._1).run(in) match {
      case Left(value) => throw value
      case Right(value) => value
    }
  }
}
