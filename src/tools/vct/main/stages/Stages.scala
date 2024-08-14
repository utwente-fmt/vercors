package vct.main.stages

import hre.progress.Progress
import vct.col.ast.{BipTransition, Program, Verification}
import vct.col.rewrite.Generation
import vct.options.Options
import vct.parsers.transform.{BlameProvider, ConstantBlameProvider}
import vct.result.VerificationError
import hre.io.{LiteralReadable, Readable}
import hre.stages.{IdentityStage, Stages}
import hre.stages.Stages.{branch, saveInput}
import vct.col.origin.{BlameCollector, VerificationFailure}
import vct.col.rewrite.bip.BIP
import vct.main.modes.VeyMont
import vct.parsers.ParseResult
import viper.api.backend.carbon.Carbon
import viper.api.backend.silicon.Silicon
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

case object Stages extends LazyLogging {
  def silicon(
      blameProvider: BlameProvider,
      bipResults: BIP.VerificationResults,
  ): Stages[Seq[Readable], Unit] = {
    Parsing(blameProvider)
      .thenRun(Resolution(blameProvider, vct.parsers.debug.DebugOptions.NONE))
      .thenRun(SilverTransformation(bipResults = bipResults))
      .thenRun(SilverBackend(Silicon())).thenRun(ExpectedErrors())
  }

  def carbon(
      blameProvider: BlameProvider,
      bipResults: BIP.VerificationResults,
  ): Stages[Seq[Readable], Unit] = {
    Parsing(blameProvider)
      .thenRun(Resolution(blameProvider, vct.parsers.debug.DebugOptions.NONE))
      .thenRun(SilverTransformation(bipResults = bipResults))
      .thenRun(SilverBackend(Carbon())).thenRun(ExpectedErrors())
  }

  /** Constructs a normal verification run from the command line options.
    *
    * A normal verification run consists of five stages: <ul> <li>[[Parsing]],
    * which transforms a set of input files to parse trees, and transforms the
    * parse trees to an unresolved abstract syntax tree (AST).</li>
    * <li>[[Resolution]], which resolves names in the AST and translates them to
    * cross-references.</li> <li>[[Transformation]], which reduces the number of
    * features in the AST so that the verification backend supports all the
    * different kinds of nodes still present.</li> <li>[[Backend]], which takes
    * in a wildly simplified AST, and verifies the specified properties. Any
    * specified properties that do not hold are reported to the respective
    * [[vct.col.origin.Blame]] members of the nodes.</li>
    * <li>[[ExpectedErrors]], which turns the absence of expected errors into
    * errors.</li> </ul>
    *
    * @param options
    * @param blameProvider
    * @return
    */
  def ofOptions(
      options: Options,
      blameProvider: BlameProvider,
      bipResults: BIP.VerificationResults,
  ): Stages[Seq[Readable], Unit] = {
    Parsing.ofOptions(options, blameProvider)
      .thenRun(Resolution.ofOptions(options, blameProvider))
      .thenRun(Transformation.ofOptions(options, bipResults))
      .thenRun(Backend.ofOptions(options))
      .thenRun(ExpectedErrors.ofOptions(options))
  }

  def veymontOfOptions(options: Options): Stages[Seq[Readable], Unit] = {
    val choreographyStage
        : Stages[Seq[Readable], Verification[_ <: Generation]] = {
      val collector = BlameCollector()
      val bipResults = BIP.VerificationResults()
      val blameProvider = ConstantBlameProvider(collector)

      IdentityStage()
        .also(logger.info("VeyMont choreography verifier & code generator"))
        .thenRun(Parsing.ofOptions(options, blameProvider).also(logger.info(
          "Finished parsing"
        ))).thenRun(Resolution.ofOptions(options, blameProvider))
        .thenRun(saveInput[Verification[_ <: Generation], Any](branch(
          !options.veymontSkipChoreographyVerification,
          IdentityStage()
            .also(logger.info("Verifying choreography with VerCors"))
            .thenRun(Transformation.ofOptions(options, bipResults))
            .thenRun(Backend.ofOptions(options))
            .thenRun(ExpectedErrors.ofOptions(options))
            .thenRun(VeyMont.NoVerificationFailures(
              collector,
              VeyMont.ChoreographyVerificationError,
            )),
          IdentityStage()
            .also(logger.warn("Skipping verifying choreography with VerCors")),
        ))).transform(_._1)
    }

    val generationStage
        : Stages[Verification[_ <: Generation], Seq[LiteralReadable]] = {
      IdentityStage().also(logger.info("Generating endpoint implementations"))
        .thenRun(Transformation.veymontImplementationGenerationOfOptions(
          options
        )).thenRun(Output.veymontOfOptions(options))
    }

    val implementationVerificationStage = {
      val collector = BlameCollector()
      val bipResults = BIP.VerificationResults()
      val blameProvider = ConstantBlameProvider(collector)

      Parsing.ofOptions(options, blameProvider)
        .thenRun(Resolution.ofOptions(options, blameProvider))
        .thenRun(Transformation.ofOptions(options, bipResults))
        .thenRun(Backend.ofOptions(options))
        .thenRun(ExpectedErrors.ofOptions(options))
        .thenRun(VeyMont.NoVerificationFailures(
          collector,
          VeyMont.ImplementationVerificationError,
        ))
    }

    choreographyStage.thenRun(generationStage)
      .thenRun(implementationVerificationStage)
  }

  def vesuvOfOptions(
      options: Options,
      blameProvider: BlameProvider,
  ): Stages[Seq[Readable], Unit] = {
    if (options.vesuvGenerateRasi) {
      Parsing.ofOptions(options, blameProvider)
        .thenRun(Resolution.ofOptions(options, blameProvider))
        .thenRun(GenerateRASI.ofOptions(options))
    } else {
      Parsing.ofOptions(options, blameProvider)
        .thenRun(Output.vesuvOfOptions(options))
    }
  }

  def cfgTransformationOfOptions(
      options: Options,
      blameProvider: BlameProvider,
  ): Stages[Seq[Readable], Unit] = {
    Parsing.ofOptions(options, blameProvider)
      .thenRun(Resolution.ofOptions(options, blameProvider))
      .thenRun(PrintCFG.ofOptions(options))
  }
}
