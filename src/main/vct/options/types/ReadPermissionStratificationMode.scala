package vct.options.types

import vct.rewrite.veymont.verification.{
  EncodePermissionStratification,
  PermissionStratificationMode => Mode,
}
import EncodePermissionStratification.Mode

case object ReadPermissionStratificationMode extends ReadEnum[Mode] {
  override val options: Map[String, Mode] = Map(
    "wrap" -> Mode.Wrap,
    "inline" -> Mode.Inline,
    "none" -> Mode.None,
  )
}
