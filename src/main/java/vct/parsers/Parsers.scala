package vct.parsers

import vct.result.VerificationResult.UserError

import java.nio.file.Path

object Parsers {
  case class UnknownFileExtension(extension: String) extends UserError {
    override def text: String = s"Unknown file extension: $extension"
    override def code: String = "unknownExt"
  }

  def getByExtension(extension: String): Parser = extension match {
    case "cl" | "c" | "cu" => ColCParser()
    case "i" => ColIParser()
    case "java" => ColJavaParser(topLevelSpecs=false)
    case "jspec" => ColJavaParser(topLevelSpecs=true)
    case "pvl" => ColPVLParser()
    case "sil" | "vpr" => ColSilverParser()
    case other => throw UnknownFileExtension(other)
  }

  def parse[G](path: Path): ParseResult[G] =
    getByExtension(path.toString.split('.').last).parse(path.toFile)()
}
