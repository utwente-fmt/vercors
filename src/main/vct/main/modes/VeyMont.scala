package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import hre.io.{CollectString, LiteralReadable, Readable}
import hre.stages.{IdentityStage, Stage, Stages}
import hre.stages.Stages.{applyIf, branch, saveInput, timed}
import hre.util.Time.logTime
import vct.col.ast.Verification
import vct.col.origin.{BlameCollector, VerificationFailure}
import vct.col.print.Ctx
import vct.col.rewrite.Generation
import vct.col.rewrite.bip.BIP
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS}
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
import vct.result.VerificationError.UserError

object VeyMont extends LazyLogging {
  case class ChoreographyVerificationError(failures: Seq[VerificationFailure])
      extends UserError {
    override def text: String = {
      val fails = failures.map(_.desc).mkString("\n")
      s"Verification of the input choreography failed because of the following failures:\n$fails"
    }

    override def code: String = "veymont:choreographyVerificationFailed"
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
                VeyMont.ChoreographyVerificationError,
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

  def verifyGenerateOptions(options: Options): Either[VerificationError, Unit] =
    logTime(
      "VeyMont mode", {
        val stages = stagesOfOptions(options)
        stages.run(options.inputs)
      },
    )

  def runOptions(options: Options): Int =
    verifyGenerateOptions(options) match {
      case Left(err: VerificationError.UserError) =>
        logger.error(err.text)
        EXIT_CODE_ERROR
      case Left(err: VerificationError.SystemError) =>
        logger.error(CollectString(s => err.printStackTrace(s)))
        EXIT_CODE_ERROR
      case Right(()) =>
        logger.info("VeyMont success")
        EXIT_CODE_SUCCESS
    }

}
