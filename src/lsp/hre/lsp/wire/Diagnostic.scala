package hre.lsp.wire

import hre.lsp.wire.Diagnostic.{CodeDescription, RelatedInformation, Severity}
import upickle.default.{macroRW, readwriter, ReadWriter => RW}

object Diagnostic {
  implicit val rwCodeDescription: RW[CodeDescription] = macroRW
  implicit val rwRelatedInformation: RW[RelatedInformation] = macroRW
  implicit val rw: RW[Diagnostic] = macroRW

  object Severity {
    implicit val rw: RW[Severity] = readwriter[Int].bimap[Severity](
      {
        case Error => 1
        case Warning => 2
        case Information => 3
        case Hint => 4
      },
      {
        case 1 => Error
        case 2 => Warning
        case 3 => Information
        case 4 => Hint
      },
    )
  }

  sealed trait Severity
  object Error extends Severity
  object Warning extends Severity
  object Information extends Severity
  object Hint extends Severity

  case class CodeDescription(href: String)

  case class RelatedInformation(location: Location, message: String)
}

case class Diagnostic(
    range: Range,
    severity: Severity = Diagnostic.Error,
    code: Option[Identifier] = None,
    codeDescription: Option[CodeDescription] = None,
    source: Option[String] = None,
    message: String,
    relatedInformation: Option[Seq[RelatedInformation]] = None,
)
