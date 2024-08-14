package hre.lsp.wire

import upickle.default.{ReadWriter => RW, macroRW}

object TextDocumentIdentifier {
  implicit val rw: RW[TextDocumentIdentifier] = macroRW
}

case class TextDocumentIdentifier(uri: DocumentUri)
