package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import hre.stages.{FunctionStage, IdentityStage, Stage, Stages}
import hre.stages.Stages.{branch, saveInput}
import hre.io.{CollectString, LiteralReadable, Readable}
import vct.col.ast.Verification
import vct.col.origin.{BlameCollector, VerificationFailure}
import vct.col.rewrite.{Generation, InitialGeneration}
import vct.col.rewrite.bip.BIP
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS, EXIT_CODE_VERIFICATION_FAILURE}
import vct.main.modes.Verify.logger
import vct.main.stages.veymont.CodeGeneration
import vct.main.stages.{Backend, ExpectedErrors, Output, Parsing, Resolution, Stages, Transformation, VeyMontImplementationGeneration}
import vct.options.Options
import vct.options.types.PathOrStd
import vct.parsers.ParseResult
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError
import vct.result.VerificationError.UserError
import viper.carbon.boogie.Implicits.lift

object VeyMont extends LazyLogging {

  case class ChoreographyVerificationError(failures: Seq[VerificationFailure]) extends UserError {
    override def text: String = {
      val fails = failures.map(_.desc).mkString("\n")
      s"Verification of the input choreography failed because of the following failures:\n$fails"
    }

    override def code: String = "veymont:choreographyVerificationFailed"
  }

  case class ImplementationVerificationError(failures: Seq[VerificationFailure]) extends UserError {
    override def text: String = {
      val fails = failures.map(_.desc).mkString("\n")
      s"Verification of the generated implementation failed because of the following failuers:\n$fails"
    }

    override def code: String = "veymont:implementationVerificationFailed"
  }

  case class NoVerificationFailures(collector: BlameCollector, error: Seq[VerificationFailure] => UserError) extends Stage[Unit, Unit] {
    override def friendlyName: String = "noVerificationErrors"
    override def progressWeight: Int = 1
    override def run(in: Unit): Unit =
      if (collector.errs.nonEmpty) {
        throw error(collector.errs.toSeq)
      }
  }

  def verifyWithOptions(options: Options, inputs: Seq[PathOrStd]) = {
    val choreographyStage: Stages[Seq[Readable], Verification[_ <: Generation]] = {
      val collector = BlameCollector()
      val bipResults = BIP.VerificationResults()
      val blameProvider = ConstantBlameProvider(collector)

      IdentityStage().also(logger.info("VeyMont choreography verifier & code generator"))
        .thenRun(Parsing.ofOptions(options, blameProvider).also(logger.info("Finished parsing")))
        .thenRun(Resolution.ofOptions(options, blameProvider))
        .thenRun(
          saveInput[Verification[_ <: Generation], Any](
            branch(!options.veymontSkipChoreographyVerification,
              IdentityStage().also(logger.info("Verifying choreography with VerCors"))
                .thenRun(Transformation.ofOptions(options, bipResults))
                .thenRun(Backend.ofOptions(options))
                .thenRun(ExpectedErrors.ofOptions(options))
                .thenRun(NoVerificationFailures(collector, ChoreographyVerificationError)),
              IdentityStage().also(logger.warn("Skipping verifying choreography with VerCors"))
            ))
        ).transform(_._1)
    }

    val generationStage: Stages[Verification[_ <: Generation], Seq[LiteralReadable]] = {
      IdentityStage().also(logger.info("Generating endpoint implementations"))
        .thenRun(Transformation.veymontImplementationGenerationOfOptions(options))
        .thenRun(Output.veymontOfOptions(options))
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
        .thenRun(NoVerificationFailures(collector, ImplementationVerificationError))
    }

    val stages = choreographyStage
      .thenRun(generationStage)
      .thenRun(implementationVerificationStage)

    stages.run(inputs) match {
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

  def runOptions(options: Options): Int = {
    verifyWithOptions(options, options.inputs)
    0
  }

}

