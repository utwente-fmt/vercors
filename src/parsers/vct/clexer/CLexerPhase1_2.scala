package vct.clexer

import java.io.{BufferedReader, Closeable}

case class CLexerPhase1_2(in: BufferedReader) extends Closeable {
  override def close(): Unit = in.close()

  private var _lineIdx: Int = 0
  private var _colIdx: Int = 0
  private var start: Pos = Pos(0, 0)

  def lineIdx: Int = _lineIdx
  def colIdx: Int = _colIdx
  def pos: Pos = Pos(lineIdx, colIdx)

  private def nextChar(): Option[Char] =
    in.read() match {
      case -1 => None
      case c =>
        val char = c.toChar
        if(char == '\n') {
          _lineIdx += 1
          _colIdx = 0
        } else {
          _colIdx += 1
        }
        Some(char)
    }

  private def range(): Range =
    Range(start, Pos(lineIdx, colIdx))

  private var buf: Seq[(Char, Range)] = Nil

  def next(): Option[(Char, Range)] = {
    start = pos

    buf match {
      case Nil => nextChar().flatMap(next)
      case c :: cs => buf = cs; Some(c)
    }
  }

  private def next(first: Char): Option[(Char, Range)] =
    first match {
      case '\\' =>
        val c2 = pos
        nextChar() match {
          case Some('\n') => next()
          case Some(c) => buf = Seq(c -> Range(c2, pos)); Some('\\' -> Range(start, c2))
        }
      case '?' =>
        val c2 = pos
        nextChar() match {
          case Some('?') =>
            val c3 = pos
            nextChar() match {
              case Some('=') => Some('#' -> range())
              case Some('(') => Some('[' -> range())
              case Some(')') => Some(']' -> range())
              case Some('<') => Some('{' -> range())
              case Some('>') => Some('}' -> range())
              case Some('/') => Some('\\' -> range())
              case Some('\'') => Some('^' -> range())
              case Some('!') => Some('|' -> range())
              case Some('-') => Some('~' -> range())
              case Some(c) => buf = Seq('?' -> Range(c2, c3), c -> Range(c3, pos)); Some('?' -> Range(start, c2))
              case None => buf = Seq('?' -> Range(c2, c3)); Some('?' -> Range(start, c2))
            }
          case Some(c) => buf = Seq(c -> Range(c2, pos)); Some('?' -> Range(start, c2))
          case None => Some('?' -> range())
        }
      case c => Some(c -> range())
    }
}
