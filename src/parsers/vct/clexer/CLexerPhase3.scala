package vct.clexer

import java.io.Closeable

case class CLexerPhase3(lexer: CLexerPhase1_2) extends Closeable {
  override def close(): Unit = lexer.close()

  private var buf: Seq[PpToken] = Nil

  def next(): Option[PpToken] = {
    buf match {
      case Nil => lexer.next().map { case (c, loc) => next(c, loc) }
      case tok :: toks => buf = toks; Some(tok)
    }
  }

  private def next(first: Char, loc: Range): PpToken =
    first match {
      case
    }
}
