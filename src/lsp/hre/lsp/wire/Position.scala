package hre.lsp.wire

import upickle.default.{ReadWriter => RW, macroRW}

object Position {
  implicit val rw: RW[Position] = macroRW
}

case class Position(line: Int, character: Int)
