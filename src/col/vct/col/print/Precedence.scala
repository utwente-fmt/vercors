package vct.col.print

object Precedence {
  val ATOMIC = 120
  val POSTFIX = 110
  val PREFIX = 100
  val SEQUENCE = 92
  val PVL_POW = 91
  val MULTIPLICATIVE = 90
  val ADDITIVE = 80
  val SHIFT = 75
  val PVL_CONTAINS = 75
  val RELATIONAL = 70
  val EQUALITY = 60
  val BIT_AND = 53
  val BIT_XOR = 52
  val BIT_OR = 51
  val AND = 50
  val OR = 40
  val IMPLIES = 30
  val SELECT = 20
  val PVL_UNFOLDING = 12
  val PVL_WITH_THEN = 11
  val ASSIGN = 10
  val UNKNOWN = 0
}
