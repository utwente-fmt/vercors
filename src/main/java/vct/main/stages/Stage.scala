package vct.main.stages

import hre.progress.Progress
import vct.col.ast.Program
import vct.col.rewrite.Generation
import vct.options.Options
import vct.parsers.transform.BlameProvider
import vct.result.VerificationError
import hre.io.Readable
import hre.stages.Stages
import viper.api.backend.carbon.Carbon
import viper.api.backend.silicon.Silicon

case object Stages {
  def silicon(blameProvider: BlameProvider): Stages[Seq[Readable], Unit] = {
    Parsing(blameProvider)
      .thenRun(Resolution(blameProvider))
      .thenRun(SilverTransformation())
      .thenRun(SilverBackend(Silicon()))
      .thenRun(ExpectedErrors())
  }

  def carbon(blameProvider: BlameProvider): Stages[Seq[Readable], Unit] = {
    Parsing(blameProvider)
      .thenRun(Resolution(blameProvider))
      .thenRun(SilverTransformation())
      .thenRun(SilverBackend(Carbon()))
      .thenRun(ExpectedErrors())
  }

  def ofOptions(options: Options, blameProvider: BlameProvider): Stages[Seq[Readable], Unit] = {
    Parsing.ofOptions(options, blameProvider)
      .thenRun(Resolution.ofOptions(options, blameProvider))
      .thenRun(Transformation.ofOptions(options))
      .thenRun(Backend.ofOptions(options))
      .thenRun(ExpectedErrors.ofOptions(options))
  }
}