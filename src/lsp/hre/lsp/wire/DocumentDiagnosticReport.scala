package hre.lsp.wire

import upickle.default.{ReadWriter => RW, macroRW}

object DocumentDiagnosticReport {
  implicit val rwUnchanged: RW[UnchangedDocumentDiagnosticReport] = macroRW
  implicit val rwFull: RW[FullDocumentDiagnosticReport] = macroRW
  implicit val rw: RW[DocumentDiagnosticReport] = RW.merge(rwUnchanged, rwFull)
}

@upickle.implicits.key("kind")
sealed trait DocumentDiagnosticReport

@upickle.implicits.key("unchanged")
case class UnchangedDocumentDiagnosticReport(resultId: String)
    extends DocumentDiagnosticReport

@upickle.implicits.key("full")
case class FullDocumentDiagnosticReport(
    resultId: Option[String] = None,
    items: Seq[Diagnostic],
) extends DocumentDiagnosticReport
