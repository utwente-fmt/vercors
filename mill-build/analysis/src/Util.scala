package vct.col.ast.analysis

import scala.util.control.NonFatal
import scala.annotation.tailrec
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

object Util {
  case class Failure(context: Seq[String], at: Option[scala.meta.Position], message: String)

  /**
   * Result is just exactly the Either monad, but flatly combines a sequence of failures. You are encouraged to
   * continue processing even when you've already failed: e.g. interpret all the declarations in Node.scala, even
   * if an earlier node is already wrong.
   *
   * - Wrap code with Try, so that you can use the unsafe methods for convenience
   * - You can call Result.get in in a Try, which is Result[T] => T
   * - You can call Seq[Result[T]].get in a Try, which is Seq[Result[T]] => Seq[T].
   *     (this is the meat and potatoes that flatly combines failures)
   * - You can call fail({node|position|nothing}, message) in a Try, which will make the Try return Failures.
   * - You can call methods that normally throw in a Try, such as Integer.parseInt.
   * - You can use check to check multiple results imperatively, to later get their values.
   */
  sealed trait Result[+T] {
    def get: T = this match {
      case f: Failures => throw f
      case ok: Ok[T] => ok.res
    }
  }

  case class Failures(failures: Seq[Failure]) extends RuntimeException with Result[Nothing] {
    require(failures.nonEmpty)
  }
  case class Ok[T](res: T) extends Result[T]

  def fail(at: scala.meta.Tree, message: String): Nothing =
    fail(at.pos, message)

  def fail(at: scala.meta.Position, message: String): Nothing =
    throw Failures(Seq(Failure(context, Some(at), message)))

  def fail(message: String): Nothing =
    throw Failures(Seq(Failure(context, at = None, message)))

  def assertPure(cond: Boolean, message: => String): Result[Unit] =
    if (cond) Ok(())
    else Failures(Seq(Failure(context, None, message)))

  def assertPure(cond: Boolean, at: => scala.meta.Tree, message: => String): Result[Unit] =
    if(cond) Ok(())
    else Failures(Seq(Failure(context, Some(at.pos), message)))

  def check(results: Result[Any]*): Unit =
    results.get

  @tailrec
  final def walkToLineStart(buf: Array[Char], pos: Int): Int =
    if(pos == 0) 0
    else if(buf(pos-1) == '\n') pos
    else walkToLineStart(buf, pos-1)

  @tailrec
  final def walkToLineEnd(buf: Array[Char], pos: Int): Int =
    if(pos == buf.size-1) buf.size-1
    else if(buf(pos) == '\n') pos
    else walkToLineEnd(buf, pos+1)

  def printFailure(failure: Failure): Unit = {
    for (context <- failure.context)
      System.err.println(s"At $context:")

    failure.at match {
      case None =>
      case Some(pos) =>
        val lineStart = walkToLineStart(pos.input.chars, pos.start)
        val lineEnd = walkToLineEnd(pos.input.chars, pos.start)

        val markerLength =
          if(pos.startLine == pos.endLine && pos.endColumn > pos.startColumn)
            pos.endColumn - pos.startColumn
          else 1

        System.err.println(s"At ${posText(pos)}:")
        System.err.println("  " + pos.input.chars.slice(lineStart, lineEnd).mkString(""))
        System.err.println("  " + " ".repeat(pos.startColumn) + "^".repeat(markerLength))
        System.err.println()
    }

    System.err.println(failure.message)
    System.err.println()
  }

  implicit class ResultSeq[T](results: Seq[Result[T]]) {
    def all: Result[Seq[T]] =
      results.foldLeft[Result[Seq[T]]](Ok(Nil)) {
        case (Failures(left), Failures(right)) => Failures(left ++ right)
        case (Failures(left), Ok(_)) => Failures(left)
        case (Ok(_), Failures(right)) => Failures(right)
        case (Ok(ts), Ok(t)) => Ok(ts :+ t)
      }

    def get: Seq[T] = all.get
  }

  implicit class SeqHelper[T](ts: Seq[T]) {
    def headOrElse[S >: T](alt: => S): S = ts.headOption.getOrElse(alt)
  }

  def posText(at: scala.meta.Tree): String = posText(at.pos)

  def posText(pos: scala.meta.Position): String = {
    val file = pos.input match {
      case scala.meta.Input.File(path, _) => path.toString
      case _ => "<unknown>"
    }
    s"$file:${pos.startLine+1}:${pos.startColumn+1}"
  }

  private var context: Seq[String] = Nil

  def Try[T](contextHere: scala.meta.Tree)(f: => T): Result[T] = Try(posText(contextHere))(f)
  def Try[T](contextHere: scala.meta.Position)(f: => T): Result[T] = Try(posText(contextHere))(f)

  /**
   * see [[Result]]
   */
  def Try[T](contextHere: String)(f: => T): Result[T] =
    try {
      context = context :+ contextHere
      Ok(f)
    } catch {
      case f: Failures => f
      case NonFatal(e) =>
        val bytes = new ByteArrayOutputStream
        val print = new PrintStream(bytes)
        e.printStackTrace(print)
        val message = new String(bytes.toByteArray, StandardCharsets.UTF_8)
        Failures(Seq(Failure(context, at = None, s"Crashed: $message")))
    } finally {
      context = context.init
    }
}