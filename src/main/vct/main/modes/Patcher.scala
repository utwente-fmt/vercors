package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import hre.progress.Progress
import hre.util.Patch
import hre.util.Patch.NoPatchException
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS}
import vct.options.Options
import vct.options.types.PathOrStd

import java.nio.file.{FileAlreadyExistsException, Files}

case object Patcher extends LazyLogging {
  def runOptions(options: Options): Int = {
    // Stages are not really necessary here, however without it Patcher might crash at the end because of a bug in Process.scala.
    // When this is fixed the stages can be safely removed.
    Progress.stages(Seq(("patching", 1))) { next =>
      val patches =
        try { Patch.fromFile(options.patchFile) }
        catch {
          case NoPatchException =>
            logger.warn("Patch file is empty, not patching anything")
            Seq()
        }

      options.inputs match {
        case Seq() =>
          logger.warn("No inputs given, not patching anything")
          EXIT_CODE_SUCCESS
        case Seq(in) =>
          logger.info(s"Applying patch `${options.patchFile}` to `${in.underlyingFile
              .getOrElse("stdin")}`. Writing result to `${options.patchOutput}`")
          Files.writeString(
            options.patchOutput,
            Patch.applyAll(patches, in.readToCompletion()),
          )
          EXIT_CODE_SUCCESS
        case inputs =>
          try { Files.createDirectories(options.patchOutput) }
          catch {
            case _: FileAlreadyExistsException =>
              logger.error("Output directory already exists as file")
              return EXIT_CODE_ERROR
          }
          for (input <- inputs) {
            val outputPath =
              input match {
                case PathOrStd.Path(path) => options.patchOutput.resolve(path)
                case PathOrStd.StdInOrOut =>
                  options.patchOutput.resolve("stdin")
              }
            logger.info(
              s"Applying patch `${options.patchFile}` to `${input.underlyingFile
                  .getOrElse("stdin")}`. Writing result to `${outputPath}`"
            )
            Files.writeString(
              outputPath,
              Patch.applyAll(patches, input.readToCompletion()),
            )
          }
          EXIT_CODE_SUCCESS
      }
    }
  }
}
