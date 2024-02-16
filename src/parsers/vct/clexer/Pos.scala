package vct.clexer

case class Pos(lineIdx: Int, colIdx: Int)
case class Range(start: Pos, end: Pos)
