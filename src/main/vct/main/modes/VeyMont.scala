package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import hre.io.{CollectString, LiteralReadable, Readable}
import hre.progress.Progress
import hre.stages.{IdentityStage, Stage, Stages}
import hre.stages.Stages.{applyIf, branch, saveInput, timed}
import hre.util.Time.logTime
import hre.util.{Time, Timer}
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
import vct.result.VerificationError.{SystemError, Unreachable, UserError}

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

  def choreographyWithOptions(
      options: Options,
      inputs: Seq[Readable],
  ): Either[VerificationError, ChoreographyResult] = {
    Progress.stages(Seq(("Parsing", 2), ("Verification", 15))) { next =>
      val collector = BlameCollector()
      val blameProvider = ConstantBlameProvider(collector)
      val timer = Timer()

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

      if (!options.veymontSkipChoreographyVerification) {
        val verificationStages = Transformation
          .ofOptions(options, BIP.VerificationResults())
          .thenRun(Backend.ofOptions(options))
          .thenRun(ExpectedErrors.ofOptions(options))

        verificationStages.run(program) match {
          case Left(error) => Left(wrap(error))
          case Right(()) =>
            val end = timer.end
            logger.info(
              s"Choreography verified successfully (duration: ${Time.formatDuration(end)})"
            )
            Right(ChoreographyResult(program, collector.errs.toSeq))
        }
      } else {
        logger.warn("Skipping choreography verification")
        Right(ChoreographyResult(program, Seq()))
      }
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
    val wrap: VerificationError => VerificationError =
      WrapVerificationError.wrap(
        "generate",
        "The following error occurred while generating an implementation:",
      )(_)
    generateStages.run(program) match {
      case Left(err) => Left(wrap(err))
      case Right(Seq(lit)) =>
        logger.info("Implementation generated successfully")
        Right(GenerateResult(lit))
      case Right(_) =>
        Left(wrap(Unreachable("Code generation should only return 1 readable")))
    }
  }

  def implementationWithOptions(
      options: Options,
      implementation: LiteralReadable,
  ): Either[VerificationError, ImplementationResult] = {
    val collector = BlameCollector()
    val bipResults = BIP.VerificationResults()
    val blameProvider = ConstantBlameProvider(collector)

    val stages = vct.main.stages.Stages.ofOptions(
      options.copy(generatePermissions = false),
      blameProvider,
      bipResults,
    )

    if (!options.veymontSkipImplementationVerification) {
      val timer = Timer()
      stages.run(Seq(implementation)) match {
        case Left(err) =>
          Left(
            WrapVerificationError.wrap(
              "implementation",
              "The following error occurred during verification of the implementation:",
            )(err)
          )
        case Right(()) =>
          val end = timer.end
          logger.info(
            s"Implementation verified successfully (duration: ${Time.formatDuration(end)})"
          )
          Right(ImplementationResult(collector.errs.toSeq))
      }
    } else {
      logger.warn("Skipping implementation verification")
      Right(ImplementationResult(collector.errs.toSeq))
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

  // VeyMont entry point for tooling. Does not do post-processing of results. inputs argument is separate such that
  // clients can inject both paths and string literals (LiteralReadable)
  def ofOptions(
      options: Options,
      inputs: Seq[Readable],
  ): Either[VerificationError, StageResult] = {
    Progress.stages(Seq(("VeyMont", 1))) { _ =>
      Progress.stages(
        Seq(("Choreography", 10), ("Generate", 2), ("Implementation", 9))
      ) { next =>
        val program =
          choreographyWithOptions(options, inputs) match {
            case Left(err) => return Left(err)
            case res @ Right(ChoreographyResult(_, failures))
                if failures.nonEmpty =>
              return res
            case Right(ChoreographyResult(program, Seq())) => program
          }

        next()

        val implementation =
          generateWithOptions(options, program)
            .fold[GenerateResult](err => return Left(err), lit => lit)
            .implementation

        next()

        implementationWithOptions(options, implementation)
      }
    }
  }

  // Main VeyMont entry point for human users. Acquires inputs from options object.
  def runOptions(options: Options): Int =
    ofOptions(options, options.inputs) match {
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
