package vct.parsers

import vct.result.VerificationResult.UserError

import java.nio.file.Path

case class FileNotFound(path: Path) extends UserError {
  override def code: String = "404"
  override def text: String = s"File not found: $path"
}
