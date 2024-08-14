package vct.options.types

sealed trait Mode

case object Mode {
  case object HelpVerifyPasses extends Mode

  case object Verify extends Mode
  case object VeyMont extends Mode
  case object VeSUV extends Mode
  case object CFG extends Mode
}
