package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import vct.col.origin.BlameCollector
import vct.col.print.Ctx
import vct.col.rewrite.bip.BIP
import vct.main.Main
import vct.main.stages.{Output, Parsing, Resolution, Transformation}
import vct.options.Options
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError

import java.nio.file.Paths

case object Compile extends LazyLogging {
  def runOptions(options: Options): Int = {
    if (options.inputs.isEmpty) {
      logger.warn("No inputs given, not compiling anything")
    }

    val collector = BlameCollector()
    val blameProvider = ConstantBlameProvider(collector)
    val result = Parsing.ofOptions(options, blameProvider)
      .thenRun(Resolution.ofOptions(options, blameProvider))
      .thenRun(Transformation.pvlJavaCompatOfOptions(options)).thenRun(Output(
        options.compileOutput.orElse(Some(Paths.get("a.java"))),
        Ctx.Java,
        false,
      )).run(options.inputs)

    result match {
      case Left(err: VerificationError.UserError) =>
        logger.error(err.text)
        Main.EXIT_CODE_ERROR
      case Left(err: VerificationError.SystemError) => throw err
      case Right(_) => Main.EXIT_CODE_SUCCESS
    }
  }
}
