package hre.lsp.wire

import upickle.default.{ReadWriter => RW, macroRW}

object DocumentDiagnosticParams {
  implicit val rw: RW[DocumentDiagnosticParams] = macroRW
}

case class DocumentDiagnosticParams(
    textDocument: TextDocumentIdentifier,
    identifier: Option[String] = None,
    previousResultId: Option[String] = None,
)
