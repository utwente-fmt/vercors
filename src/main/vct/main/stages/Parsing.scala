package vct.main.stages

import hre.io.Readable
import hre.progress.Progress
import hre.stages.Stage
import vct.col.origin.{Origin, ReadableOrigin}
import vct.col.rewrite.Generation
import vct.main.stages.Parsing.{Language, UnknownFileExtension}
import vct.options.Options
import vct.parsers._
import vct.parsers.debug.DebugOptions
import vct.parsers.parser.{
  ColCPPParser,
  ColCParser,
  ColIPPParser,
  ColIParser,
  ColJavaParser,
  ColLLVMParser,
  ColPVLParser,
  ColSystemCParser,
}
import vct.parsers.transform.BlameProvider
import vct.resources.Resources
import vct.result.VerificationError.UserError
import viper.api.transform.ColSilverParser

import java.nio.file.{Path, Paths}

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

  def ofOptions[G <: Generation](
      options: Options,
      blameProvider: BlameProvider,
  ): Parsing[G] =
    Parsing(
      blameProvider = blameProvider,
      debugOptions = options.getParserDebugOptions,
      forceLanguage = options.language,
      cc = options.cc,
      cSystemInclude = options.cIncludePath,
      cOtherIncludes = Nil,
      cDefines = options.cDefine,
    )
}

case class Parsing[G <: Generation](
    blameProvider: BlameProvider,
    forceLanguage: Option[Language] = None,
    debugOptions: DebugOptions = DebugOptions.NONE,
    cc: Path = Resources.getCcPath,
    cSystemInclude: Path = Resources.getCIncludePath,
    cOtherIncludes: Seq[Path] = Nil,
    cDefines: Map[String, String] = Map.empty,
    ccpp: Path = Resources.getCPPcPath,
    cppSystemInclude: Path = Resources.getCPPIncludePath,
    cppOtherIncludes: Seq[Path] = Nil,
    cppDefines: Map[String, String] = Map.empty,
) extends Stage[Seq[Readable], ParseResult[G]] {
  override def friendlyName: String = "Parsing"
  override def progressWeight: Int = 4

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
                debugOptions,
                blameProvider,
                cc,
                cSystemInclude,
                Option(Paths.get(readable.fileName).getParent).toSeq ++
                  cOtherIncludes,
                cDefines,
              )
            case Language.InterpretedC =>
              ColIParser(debugOptions, blameProvider, cOrigin = None)
            case Language.CPP =>
              ColCPPParser(
                debugOptions,
                blameProvider,
                ccpp,
                cppSystemInclude,
                Option(Paths.get(readable.fileName).getParent).toSeq ++
                  cppOtherIncludes,
                cppDefines,
              )
            case Language.InterpretedCPP =>
              ColIPPParser(debugOptions, blameProvider, cppOrigin = None)
            case Language.Java => ColJavaParser(debugOptions, blameProvider)
            case Language.PVL => ColPVLParser(debugOptions, blameProvider)
            case Language.Silver => ColSilverParser(blameProvider)
            case Language.SystemC =>
              new ColSystemCParser(Resources.getSystemCConfig)
            case Language.LLVM =>
              ColLLVMParser(debugOptions, blameProvider, Resources.getPallas)
          }

        parser.parse[G](readable)
      }.iterator.to(Seq)
    )
}
