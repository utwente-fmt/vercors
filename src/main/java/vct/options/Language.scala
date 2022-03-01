package vct.options

sealed trait Language

case object Language {
  case object Java extends Language
  case object C extends Language
  case object Cuda extends Language
  case object PVL extends Language
  case object Silver extends Language
}
