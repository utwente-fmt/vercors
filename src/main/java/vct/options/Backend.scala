package vct.options

sealed trait Backend

case object Backend {
  case object Silicon extends Backend
  case object Carbon extends Backend
}