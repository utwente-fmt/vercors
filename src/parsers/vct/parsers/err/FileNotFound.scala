package vct.parsers.err

import vct.result.VerificationError.UserError

case class FileNotFound(path: String) extends UserError {
  override def code: String = "404"
  override def text: String = s"File not found: $path"
}
