package hre.lsp.wire

import upickle.default.{ReadWriter => RW, macroRW}

object Range {
  implicit val rw: RW[Range] = macroRW
}

case class Range(start: Position, end: Position)
