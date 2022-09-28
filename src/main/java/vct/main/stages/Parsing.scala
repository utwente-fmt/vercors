package vct.main.stages

import hre.io.Readable
import hre.stages.Stage
import vct.col.rewrite.Generation
import vct.main.stages.Parsing.{Language, UnknownFileExtension}
import vct.options.Options
import vct.parsers.transform.{BlameProvider, ReadableOriginProvider}
import vct.parsers._
import vct.resources.Resources
import vct.result.VerificationError.UserError
import viper.api
import viper.api.transform.ColSilverParser

import java.nio.file.Path

case object Parsing {
  sealed trait Language

  case object Language {
    def fromFilename(filename: String): Option[Language] =
      filename.split('.').last match {
        case "cl" | "c" | "cu" => Some(C)
        case "i" => Some(InterpretedC)
        case "java" => Some(Java)
        case "pvl" => Some(PVL)
        case "sil" | "vpr" => Some(Silver)
        case _ => None
      }

    case object C extends Language
    case object InterpretedC extends Language
    case object Java extends Language
    case object PVL extends Language
    case object Silver extends Language
  }

  case class UnknownFileExtension(extension: String) extends UserError {
    override def text: String = s"Unknown file extension: $extension. Try altering the extension of the file, or specify a language explicitly with --lang."
    override def code: String = "unknownExt"
  }

  def ofOptions[G <: Generation](options: Options, blameProvider: BlameProvider): Parsing[G] =
    Parsing(
      blameProvider = blameProvider,
      forceLanguage = options.language,
      cc = options.cc,
      cSystemInclude = options.cIncludePath,
      cOtherIncludes = Nil,
      cDefines = options.cDefine,
    )
}

case class Parsing[G <: Generation]
(
  blameProvider: BlameProvider,
  forceLanguage: Option[Language] = None,
  cc: Path = Resources.getCcPath,
  cSystemInclude: Path = Resources.getCIncludePath,
  cOtherIncludes: Seq[Path] = Nil,
  cDefines: Map[String, String] = Map.empty,
) extends Stage[Seq[Readable], ParseResult[G]] {
  override def friendlyName: String = "Parsing"
  override def progressWeight: Int = 4

  override def run(in: Seq[Readable]): ParseResult[G] =
    ParseResult.reduce(in.map { readable =>
      val language = forceLanguage
        .orElse(Language.fromFilename(readable.fileName))
        .getOrElse(throw UnknownFileExtension(readable.fileName))

      val originProvider = ReadableOriginProvider(readable)

      val parser = language match {
        case Language.C => ColCParser(originProvider, blameProvider, cc, cSystemInclude, cOtherIncludes, cDefines)
        case Language.InterpretedC => ColIParser(originProvider, blameProvider)
        case Language.Java => ColJavaParser(originProvider, blameProvider)
        case Language.PVL => ColPVLParser(originProvider, blameProvider)
        case Language.Silver => ColSilverParser(originProvider, blameProvider)
      }

      parser.parse[G](readable)
    })
}