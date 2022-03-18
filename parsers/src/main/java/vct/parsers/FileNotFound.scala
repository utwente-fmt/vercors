package vct.parsers

import vct.result.VerificationError.UserError

import java.nio.file.Path

case class FileNotFound(path: String) extends UserError {
  override def code: String = "404"
  override def text: String = s"File not found: $path"
}
