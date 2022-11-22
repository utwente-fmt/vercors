package vct.main.stages

import hre.progress.Progress
import vct.col.ast.{BipTransition, Program}
import vct.col.rewrite.Generation
import vct.options.Options
import vct.parsers.transform.BlameProvider
import vct.result.VerificationError
import hre.io.Readable
import hre.stages.Stages
import vct.col.rewrite.bip.{BipVerificationResult, BipVerificationResults}
import viper.api.backend.carbon.Carbon
import viper.api.backend.silicon.Silicon

import scala.collection.mutable

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

  /**
   * Constructs a normal verification run from the command line options.
   *
   * A normal verification run consists of five stages:
   * <ul>
   *   <li>[[Parsing]], which transforms a set of input files to parse trees, and transforms the parse trees to an
   *   unresolved abstract syntax tree (AST).</li>
   *   <li>[[Resolution]], which resolves names in the AST and translates them to cross-references.</li>
   *   <li>[[Transformation]], which reduces the number of features in the AST so that the verification backend
   *   supports all the different kinds of nodes still present.</li>
   *   <li>[[Backend]], which takes in a wildly simplified AST, and verifies the specified properties. Any specified
   *   properties that do not hold are reported to the respective [[vct.col.origin.Blame]] members of the nodes.</li>
   *   <li>[[ExpectedErrors]], which turns the absence of expected errors into errors.</li>
   * </ul>
   *
   * @param options
   * @param blameProvider
   * @return
   */
  def ofOptions(options: Options, blameProvider: BlameProvider, bipResults: BipVerificationResults): Stages[Seq[Readable], Unit] = {
    Parsing.ofOptions(options, blameProvider)
      .thenRun(Resolution.ofOptions(options, blameProvider))
      .thenRun(Transformation.ofOptions(options, bipResults))
      .thenRun(Backend.ofOptions(options))
      .thenRun(ExpectedErrors.ofOptions(options))
  }
}