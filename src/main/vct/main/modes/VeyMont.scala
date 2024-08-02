package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import hre.io.{CollectString, LiteralReadable, Readable}
import hre.progress.Progress
import hre.stages.{IdentityStage, Stage, Stages}
import hre.stages.Stages.{applyIf, branch, saveInput, timed}
import hre.util.Time.logTime
import vct.col.ast.Verification
import vct.col.origin.{BlameCollector, TableEntry, VerificationFailure}
import vct.col.print.Ctx
import vct.col.rewrite.Generation
import vct.col.rewrite.bip.BIP
import vct.main.Main.{
  EXIT_CODE_ERROR,
  EXIT_CODE_SUCCESS,
  EXIT_CODE_VERIFICATION_FAILURE,
}
import vct.main.modes.Verify.logger
import vct.main.stages.{
  Backend,
  ExpectedErrors,
  Output,
  Parsing,
  Resolution,
  Transformation,
}
import vct.options.Options
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError
import vct.result.VerificationError.{SystemError, UserError}

object VeyMont extends LazyLogging {
  case object WrapVerificationError {
    def wrap(codePrefix: String, textPrefix: String)(
        error: VerificationError
    ): VerificationError =
      error match {
        case error: UserError => WrapUserError(codePrefix, textPrefix, error)
        case error: SystemError => WrapSystemError(textPrefix, error)
      }
  }
  case class WrapUserError(
      codePrefix: String,
      textPrefix: String,
      error: UserError,
  ) extends UserError {
    override def code: String = s"$codePrefix:${error.code}"
    override def text: String = s"$textPrefix\n${error.text}"
  }
  case class WrapSystemError(textPrefix: String, error: SystemError)
      extends SystemError {
    override def text: String = s"$textPrefix\n${error.text}"
  }

  case class ImplementationVerificationError(failures: Seq[VerificationFailure])
      extends UserError {
    override def text: String = {
      val fails = failures.map(_.desc).mkString("\n")
      s"Verification of the generated implementation failed because of the following failuers:\n$fails"
    }

    override def code: String = "veymont:implementationVerificationFailed"
  }

  case class NoVerificationFailures(
      collector: BlameCollector,
      error: Seq[VerificationFailure] => UserError,
  ) extends Stage[Unit, Unit] {
    override def friendlyName: String = "noVerificationErrors"
    override def progressWeight: Int = 1
    override def run(in: Unit): Unit =
      if (collector.errs.nonEmpty) { throw error(collector.errs.toSeq) }
  }

  def choreographyWithOptions(
      options: Options,
      inputs: Seq[Readable],
  ): Either[VerificationError, ChoreographyResult] = {
    Progress.stages(Seq(("Parsing", 2), ("Verification", 15))) { next =>
      val collector = BlameCollector()
      val blameProvider = ConstantBlameProvider(collector)
      // TODO (RR): delete veymontGeneratePermissions to have just one
      val parsingStages = Parsing.ofOptions(options, blameProvider)
        .thenRun(Resolution.ofOptions(options, blameProvider))

      val wrap: VerificationError => VerificationError = WrapVerificationError
        .wrap(
          "choreography",
          "The following error occurred during choreography verification:",
        )

      val program =
        parsingStages.run(inputs) match {
          case Left(err) => return Left(wrap(err))
          case Right(program) => program
        }

      next()

      val verificationStages = Transformation
        .ofOptions(options, BIP.VerificationResults())
        .thenRun(Backend.ofOptions(options))
        .thenRun(ExpectedErrors.ofOptions(options))

      verificationStages.run(program) match {
        case Left(error) => Left(wrap(error))
        case Right(()) =>
          Right(ChoreographyResult(program, collector.errs.toSeq))
      }

      // TODO (RR): Reinstate this log?
//          logger.info("Choreographic verification successful ✔\uFE0F")
//              ),
//            ))).transform(_._1)
//          }
    }
  }

  def generateWithOptions(
      options: Options,
      program: Verification[_ <: Generation],
  ): Either[VerificationError, GenerateResult] = {
    val generateJava = options.veymontOutput
      .exists(_.toString.endsWith(".java"))
    val generateStages = Transformation
      .veymontImplementationGenerationOfOptions(options)
      .thenRun(applyIf[Verification[_ <: Generation]](
        generateJava,
        IdentityStage().thenRun(Transformation.pvlJavaCompatOfOptions(options)),
      )).thenRun(Output(
        options.veymontOutput,
        if (generateJava)
          Ctx.Java
        else
          Ctx.PVL,
        false,
      ))
    generateStages.run(program) match {
      case Left(err) =>
        Left(
          WrapVerificationError.wrap(
            "generate",
            "The following error occurred while generating an implementation:",
          )(err)
        )
      case Right(Seq(lit)) => Right(GenerateResult(lit))
      case Right(_) => ??? // Should always be only 1 literal readable
    }
  }

  def implementationWithOptions(
      options: Options,
      implementation: LiteralReadable,
  ): Either[VerificationError, ImplementationResult] = {
    val collector = BlameCollector()
    val bipResults = BIP.VerificationResults()
    val blameProvider = ConstantBlameProvider(collector)
    // TODO: Delete?
//    val options = globalOptions.copy(generatePermissions = false)

    val stages = vct.main.stages.Stages
      .ofOptions(options, blameProvider, bipResults)
      .thenRun(VeyMont.NoVerificationFailures(
        collector,
        VeyMont.ImplementationVerificationError,
      ))

    stages.run(Seq(implementation)) match {
      case Left(err) =>
        Left(
          WrapVerificationError.wrap(
            "implementation",
            "The following error occurred during verification of the implementation:",
          )(err)
        )
      case Right(()) => Right(ImplementationResult(collector.errs.toSeq))
    }
  }

  sealed trait StageResult
  case class ChoreographyResult(
      program: Verification[_ <: Generation],
      failures: Seq[VerificationFailure],
  ) extends StageResult
  case class GenerateResult(implementation: LiteralReadable) extends StageResult
  case class ImplementationResult(failures: Seq[VerificationFailure])
      extends StageResult

  def ofOptions(options: Options): Either[VerificationError, StageResult] = {
    Progress.stages(Seq(("VeyMont", 1))) { _ =>
      Progress.stages(
        Seq(("Choreography", 10), ("Generate", 2), ("Implementation", 9))
      ) { next =>
        // TODO: record choreographyphase in error
        val program =
          choreographyWithOptions(options, options.inputs) match {
            case Left(err) => return Left(err)
            case res @ Right(ChoreographyResult(_, failures))
                if failures.nonEmpty =>
              return res
            case Right(ChoreographyResult(program, Seq())) => program
          }

        logger.info("Choreographic verification success")

        next()

        val implementation =
          generateWithOptions(options, program)
            .fold[GenerateResult](err => return Left(err), lit => lit)
            .implementation

        logger.info("Implementation generation success")

        next()

        implementationWithOptions(options, implementation)
      }
    }
  }

  def stagesOfOptions(globalOptions: Options): Stages[Seq[Readable], Unit] = {
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
//                VeyMont.ChoreographyVerificationError,
                ???,
              )).also(logger.info(
                "Choreographic verification successful ✔\uFE0F"
              )),
          ),
          IdentityStage()
            .also(logger.warn("Skipping verifying choreography with VerCors")),
        ))).transform(_._1)
    }

    val generateJava = globalOptions.veymontOutput
      .exists(_.toString.endsWith(".java"))
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
          )).thenRun(Output(
            options.veymontOutput,
            if (generateJava)
              Ctx.Java
            else
              Ctx.PVL,
            false,
          )),
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
          vct.main.stages.Stages.ofOptions(options, blameProvider, bipResults)
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

  def verifyGenerateOptions(
      options: Options
  ): Either[VerificationError, Unit] = {
    // TODO (RR): Skipping, focusing, etc.
    logTime(
      "VeyMont mode", {
        Progress.stages(Seq(("VeyMont", 1))) { _ =>
          Progress.stages(Seq(
            ("Choreography", 10),
            ("Generation", 2),
            ("Implementation verification", 9),
          )) { next =>
            val stages = stagesOfOptions(options)
            stages.run(options.inputs)
          }
        }
      },
    )
  }

  def runOptions(options: Options): Int =
    ofOptions(options) match {
      case Left(err: VerificationError.UserError) =>
        logger.error(err.text)
        EXIT_CODE_ERROR
      case Left(err: VerificationError.SystemError) =>
        logger.error(CollectString(s => err.printStackTrace(s)))
        EXIT_CODE_ERROR
      case Right(result) =>
        result match {
          case ChoreographyResult(_, fails) if fails.nonEmpty =>
            logger.error("Verification of choreography failed")
            if (options.more || fails.size <= 2)
              fails.foreach(fail => logger.error(fail.desc))
            else {
              logger.info(
                "Printing verification results as a compressed table. Run with `--more` for verbose verification results."
              )
              logger.error(TableEntry.render(fails.map(_.asTableEntry)))
            }
            EXIT_CODE_VERIFICATION_FAILURE
          case ChoreographyResult(_, _) => EXIT_CODE_SUCCESS
          case GenerateResult(_) => EXIT_CODE_SUCCESS
          case ImplementationResult(fails) if fails.nonEmpty =>
            logger.error("Verification of generated implementation failed")
            if (options.more || fails.size <= 2)
              fails.foreach(fail => logger.error(fail.desc))
            else {
              logger.info(
                "Printing verification results as a compressed table. Run with `--more` for verbose verification results."
              )
              logger.error(TableEntry.render(fails.map(_.asTableEntry)))
            }
            EXIT_CODE_VERIFICATION_FAILURE
          case ImplementationResult(_) => EXIT_CODE_SUCCESS
        }
    }

}
