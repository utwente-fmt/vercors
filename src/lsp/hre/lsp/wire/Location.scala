package hre.lsp.wire

import upickle.default.{ReadWriter => RW, macroRW}

object Location {
  implicit val rw: RW[Location] = macroRW
}

case class Location(uri: DocumentUri, range: Range)
