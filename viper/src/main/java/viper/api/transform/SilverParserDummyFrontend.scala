package viper.api.transform

import hre.io.Readable
import viper.silver.ast.Program
import viper.silver.frontend.{DefaultStates, SilFrontend, SilFrontendConfig}
import viper.silver.reporter.Reporter
import viper.silver.verifier.{AbstractError, Verifier}

import java.nio.file.{Files, Path, Paths}
import scala.annotation.nowarn
import scala.io.Source

// We can only refactor this once silver starts using trait parameters (or the suggested workaround)
// So we silence the warning because it is not useful.
@nowarn("msg=.*early initializers are deprecated.*")
object SilverParserDummyFrontend extends {
  // early initializer: reporter must be populated before initialization of superclass SilFrontend
  override val reporter: Reporter = NopViperReporter
} with SilFrontend {
  private val noVerifier: viper.silver.verifier.NoVerifier = new viper.silver.verifier.NoVerifier

  def parse(input: String, diagnosticsPath: Path): Either[Seq[AbstractError], Program] = {
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
    parse(Source.fromInputStream(Files.newInputStream(path)).mkString, path)

  def parse(readable: Readable): Either[Seq[AbstractError], Program] =
    parse(readable.readToCompletion(), Paths.get(readable.fileName))

  override def createVerifier(fullCmd: String): Verifier = noVerifier

  case object SilverParserDummyConfig extends SilFrontendConfig(Nil, "dummy")
  override def configureVerifier(args: Seq[String]): SilFrontendConfig = SilverParserDummyConfig
}
