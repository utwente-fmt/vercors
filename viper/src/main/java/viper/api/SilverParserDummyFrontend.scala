package viper.api

import viper.silver.ast.Program
import viper.silver.frontend.{SilFrontend, SilFrontendConfig}
import viper.silver.plugin.PluginAwareReporter
import viper.silver.verifier.{AbstractError, Verifier}

import java.nio.file.Path
import scala.annotation.nowarn

// We can only refactor this once silver starts using trait parameters (or the suggested workaround)
// So we silence the warning because it is not useful.
@nowarn("msg=.*early initializers are deprecated.*")
object SilverParserDummyFrontend extends {
  // early initializer: reporter must be populated before initialization of superclass SilFrontend
  override val reporter: PluginAwareReporter = PluginAwareReporter(HREViperReporter())
} with SilFrontend {
  private val silicon: viper.silver.verifier.NoVerifier = new viper.silver.verifier.NoVerifier

  def parse(path: Path): Either[Seq[AbstractError], Program] = {
    init(silicon)
    reset(path)
    parsing()
    semanticAnalysis()
    translation()

    program match {
      case Some(program) => Right(program)
      case None => Left(errors)
    }
  }

  override def createVerifier(fullCmd: String): Verifier = silicon

  case object SilverParserDummyConfig extends SilFrontendConfig(Nil, "dummy")
  override def configureVerifier(args: Seq[String]): SilFrontendConfig = SilverParserDummyConfig
}
