package hre.lsp.wire

import upickle.default.{ReadWriter => RW, macroRW}

object ResponseError {
  implicit val rw: RW[ResponseError] = macroRW
}

case class ResponseError(code: Int, message: String)
