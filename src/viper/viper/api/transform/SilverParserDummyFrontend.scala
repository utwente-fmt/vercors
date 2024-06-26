package viper.api.transform

import hre.io.{RWFile, Readable}
import viper.silver.ast.Program
import viper.silver.frontend.{DefaultStates, SilFrontend, SilFrontendConfig}
import viper.silver.reporter.Reporter
import viper.silver.verifier.{AbstractError, Verifier}

import java.nio.file.{Path, Paths}
import scala.annotation.nowarn

// We can only refactor this once silver starts using trait parameters (or the suggested workaround)
// So we silence the warning because it is not useful.
@nowarn("msg=.*early initializers are deprecated.*")
class SilverParserDummyFrontend(
    override val reporter: Reporter = NopViperReporter
) extends SilFrontend {
  private val noVerifier: viper.silver.verifier.NoVerifier =
    new viper.silver.verifier.NoVerifier

  def parse(
      input: String,
      diagnosticsPath: Path,
  ): Either[Seq[AbstractError], Program] = {
    init(noVerifier)

    _state = DefaultStates.InputSet
    _inputFile = Some(diagnosticsPath)
    _input = Some(input)
    _errors = Seq()
    _parsingResult = None
    _semanticAnalysisResult = None
    _verificationResult = None
    _program = None
    resetMessages()

    parsing()
    semanticAnalysis()
    translation()

    program match {
      case Some(program) => Right(program)
      case None => Left(errors)
    }
  }

  def parse(path: Path): Either[Seq[AbstractError], Program] =
    parse(RWFile(path).readToCompletion(), path)

  def parse(readable: Readable): Either[Seq[AbstractError], Program] =
    parse(readable.readToCompletion(), Paths.get(readable.fileName))

  override def createVerifier(fullCmd: String): Verifier = noVerifier

  case object SilverParserDummyConfig extends SilFrontendConfig(Nil, "dummy")
  override def configureVerifier(args: Seq[String]): SilFrontendConfig =
    SilverParserDummyConfig
}
