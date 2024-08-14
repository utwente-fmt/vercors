package vct.options

import vct.main.stages.Parsing.Language
import vct.resources.Resources

import java.nio.file.{Path, Paths}

case class ParsingOptions(
    devParserReportAmbiguities: Boolean = false,
    devParserReportContextSensitivities: Boolean = false,
    language: Option[Language] = None,
    cc: Path = Paths.get("clang"),
    cIncludePath: Path = Resources.getCIncludePath,
    cOtherIncludePaths: Seq[Path] = Nil,
    cDefine: Map[String, String] = Map.empty,
    ccpp: Path = Paths.get("clang++"),
    cppIncludePath: Path = Resources.getCPPIncludePath,
    cppOtherIncludePaths: Seq[Path] = Nil,
    cppDefine: Map[String, String] = Map.empty,
)
