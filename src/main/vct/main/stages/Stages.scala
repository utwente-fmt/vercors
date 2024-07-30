package vct.main.stages

import hre.progress.Progress
import vct.col.ast.{BipTransition, Program, Verification}
import vct.col.rewrite.Generation
import vct.options.Options
import vct.parsers.transform.{BlameProvider, ConstantBlameProvider}
import vct.result.VerificationError
import hre.io.{LiteralReadable, Readable}
import hre.stages.{IdentityStage, Stages}
import hre.stages.Stages.{branch, saveInput, timed, applyIf}
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

  def veymontOfOptions(globalOptions: Options): Stages[Seq[Readable], Unit] = {
    // generatePermissions is precluded by veymontGeneratePermissions in VeyMont mode
    if (globalOptions.generatePermissions) {
      logger.warn(
        "`--generate-permissions` does not do anything in VeyMont mode, please use --veymont-generate-permissions"
      )
    }

    val choreographyStage
        : Stages[Seq[Readable], Verification[_ <: Generation]] = {
      val collector = BlameCollector()
      val bipResults = BIP.VerificationResults()
      val blameProvider = ConstantBlameProvider(collector)
      val options = globalOptions
        .copy(generatePermissions = globalOptions.veymontGeneratePermissions)

      Parsing.ofOptions(options, blameProvider)
        .thenRun(Resolution.ofOptions(options, blameProvider))
        .thenRun(saveInput[Verification[_ <: Generation], Any](branch(
          !options.veymontSkipChoreographyVerification,
          timed(
            "choreographic verification with VerCors",
            Transformation.ofOptions(options, bipResults)
              .thenRun(Backend.ofOptions(options))
              .thenRun(ExpectedErrors.ofOptions(options))
              .thenRun(VeyMont.NoVerificationFailures(
                collector,
                VeyMont.ChoreographyVerificationError,
              )).also(logger.info(
                "Choreographic verification successful ✔\uFE0F"
              )),
          ),
          IdentityStage()
            .also(logger.warn("Skipping verifying choreography with VerCors")),
        ))).transform(_._1)
    }

    val generateJava = globalOptions.veymontOutput.exists(_.endsWith(".java"))
    val generationStage
        : Stages[Verification[_ <: Generation], Seq[LiteralReadable]] = {
      val options = globalOptions
      timed(
        "generating endpoint implementations",
        Transformation.veymontImplementationGenerationOfOptions(options)
          .thenRun(applyIf[Verification[_ <: Generation]](
            generateJava,
            IdentityStage()
              .thenRun(Transformation.pvlJavaCompatOfOptions(options)),
          )).thenRun(Output.veymontOfOptions(options)),
      )
    }

    val implementationVerificationStage = {
      val collector = BlameCollector()
      val bipResults = BIP.VerificationResults()
      val blameProvider = ConstantBlameProvider(collector)
      // generatePermissions is precluded by veymontGeneratePermissions in VeyMont mode
      val options = globalOptions.copy(generatePermissions = false)

      branch(
        !options.veymontSkipImplementationVerification,
        timed(
          "verify generated implementation with VerCors",
          Stages.ofOptions(options, blameProvider, bipResults)
            .thenRun(VeyMont.NoVerificationFailures(
              collector,
              VeyMont.ImplementationVerificationError,
            )),
        ),
        IdentityStage().drop()
          .also(logger.warn("Skipping implementation verification")),
      )
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
