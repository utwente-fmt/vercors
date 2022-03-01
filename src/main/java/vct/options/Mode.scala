package vct.options

sealed trait Mode

case object Mode {
  case object Verify extends Mode
  case object VeyMont extends Mode
  case object BatchTest extends Mode
}
