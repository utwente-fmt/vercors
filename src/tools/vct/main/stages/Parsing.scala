package vct.main.stages

import hre.io.Readable
import hre.progress.Progress
import hre.stages.Stage
import vct.col.origin.{Origin, ReadableOrigin}
import vct.col.rewrite.Generation
import vct.main.stages.Parsing.{Language, UnknownFileExtension}
import vct.options.ParsingOptions
import vct.parsers._
import vct.parsers.debug.DebugOptions
import vct.parsers.parser._
import vct.parsers.transform.BlameProvider
import vct.resources.Resources
import vct.result.VerificationError.UserError
import viper.api.transform.ColSilverParser

import java.nio.file.Paths

case object Parsing {
  sealed trait Language

  case object Language {
    def fromFilename(filename: String): Option[Language] =
      filename.split('.').last match {
        case "cl" | "c" | "cu" => Some(C)
        case "cpp" => Some(CPP)
        case "i" => Some(InterpretedC)
        case "ipp" => Some(InterpretedCPP)
        case "java" => Some(Java)
        case "pvl" => Some(PVL)
        case "sil" | "vpr" => Some(Silver)
        case "xml" => Some(SystemC)
        case "ll" => Some(LLVM)
        case _ => None
      }

    case object C extends Language
    case object InterpretedC extends Language
    case object CPP extends Language
    case object InterpretedCPP extends Language
    case object Java extends Language
    case object PVL extends Language
    case object Silver extends Language
    case object SystemC extends Language
    case object LLVM extends Language
  }

  case class UnknownFileExtension(extension: String) extends UserError {
    override def text: String =
      s"Unknown file extension: $extension. Try altering the extension of the file, or specify a language explicitly with --lang."
    override def code: String = "unknownExt"
  }
}

case class Parsing[G <: Generation](
    blameProvider: BlameProvider,
    options: ParsingOptions,
) extends Stage[Seq[Readable], ParseResult[G]] {
  override def friendlyName: String = "Parsing"
  override def progressWeight: Int = 4

  def parserDebugOptions: DebugOptions =
    DebugOptions(
      options.devParserReportAmbiguities,
      options.devParserReportContextSensitivities,
    )

  override def run(in: Seq[Readable]): ParseResult[G] =
    ParseResult.reduce(
      Progress.map(in, (r: Readable) => r.fileName) { readable =>
        val language = forceLanguage
          .orElse(Language.fromFilename(readable.fileName))
          .getOrElse(throw UnknownFileExtension(readable.fileName))

        val origin = Origin(Seq(ReadableOrigin(readable)))

        val parser =
          language match {
            case Language.C =>
              ColCParser(
                parserDebugOptions,
                blameProvider,
                options.cc,
                options.cIncludePath,
                Option(Paths.get(readable.fileName).getParent).toSeq ++
                  options.cOtherIncludePaths,
                options.cDefine,
              )
            case Language.InterpretedC =>
              ColIParser(parserDebugOptions, blameProvider, cOrigin = None)
            case Language.CPP =>
              ColCPPParser(
                DebugOptions(
                  options.devParserReportAmbiguities,
                  options.devParserReportContextSensitivities,
                ),
                blameProvider,
                options.ccpp,
                options.cppIncludePath,
                Option(Paths.get(readable.fileName).getParent).toSeq ++
                  options.cppOtherIncludePaths,
                options.cppDefine,
              )
            case Language.InterpretedCPP =>
              ColIPPParser(parserDebugOptions, blameProvider, cppOrigin = None)
            case Language.Java =>
              ColJavaParser(parserDebugOptions, blameProvider)
            case Language.PVL => ColPVLParser(parserDebugOptions, blameProvider)
            case Language.Silver => ColSilverParser(blameProvider)
            case Language.SystemC =>
              new ColSystemCParser(Resources.getSystemCConfig)
            case Language.LLVM =>
              ColLLVMParser(
                parserDebugOptions,
                blameProvider,
                Resources.getVCLLVM,
              )
          }

        parser.parse[G](readable)
      }.iterator.to(Seq)
    )
}
