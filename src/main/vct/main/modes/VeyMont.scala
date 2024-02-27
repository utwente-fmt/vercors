package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import hre.stages.{Stage, Stages}
import hre.stages.StagesHelpers._
import hre.io.Readable
import vct.col.ast.Verification
import vct.col.origin.{BlameCollector, VerificationFailure}
import vct.col.rewrite.{Generation, InitialGeneration}
import vct.col.rewrite.bip.BIP
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS, EXIT_CODE_VERIFICATION_FAILURE}
import vct.main.modes.Verify.logger
import vct.main.stages.veymont.CodeGeneration
import vct.main.stages.{Backend, ExpectedErrors, Output, Parsing, Resolution, Stages, StringReadable, Transformation, VeyMontImplementationGeneration}
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

  object VerificationStore {
    var v: Verification[_] = null
  }

  case class SetVerification() extends Stage[Verification[_ <: Generation], Verification[_ <: Generation]] {
    override def friendlyName: String = "setVerification"
    override def progressWeight: Int = 1
    override def run(in: Verification[_ <: Generation]): Verification[_ <: Generation] = {
      VerificationStore.v = in
      in
    }
  }

  case class GetVerification() extends Stage[Unit, Verification[_ <: Generation]] {
    override def friendlyName: String = "getVerification"
    override def progressWeight: Int = 1
    override def run(in: Unit): Verification[_ <: Generation] = VerificationStore.v.asInstanceOf
  }

  def verifyWithOptions(options: Options, inputs: Seq[PathOrStd]) = {
    // TODO (RR): Refactor with FunctionStage

    val choreographyStage: Stages[Seq[Readable], Unit] = {
      val collector = BlameCollector()
      val bipResults = BIP.VerificationResults()
      val blameProvider = ConstantBlameProvider(collector)

      Parsing.ofOptions(options, blameProvider)
        .thenRun(Resolution.ofOptions(options, blameProvider))
        .thenRun(SetVerification())
        .thenRun(Transformation.ofOptions(options, bipResults))
        .thenRun(Backend.ofOptions(options))
        .thenRun(ExpectedErrors.ofOptions(options))
        .thenRun(NoVerificationFailures(collector, ChoreographyVerificationError))
    }

    val generationStage: Stages[Unit, Seq[StringReadable]] = GetVerification()
      .thenRun(Transformation.veymontImplementationGenerationOfOptions(options))
      .thenRun(Output.veymontOfOptions(options))

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
      case Left(value) => logger.error(value.text)
      case Right(()) => logger.info("VeyMont success")
    }
  }

  def runOptions(options: Options): Int = {
    verifyWithOptions(options, options.inputs)
    0
  }

}

