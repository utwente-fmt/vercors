package scala.meta

import java.io.Writer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.meta.internal.prettyprinters.TreeSyntax
import scala.meta.prettyprinters.Show._
import scala.meta.dialects.Scala213
import scala.util.Using

object ResultStream {
  /**
   * Forwards all writes to the inner Appenable, but stores the two last written
   * characters.
   */
  class MiniContextAppendable(inner: Appendable) extends Appendable {
    private var last: Char = ' '
    private var prelast: Char = ' '

    override def append(cs: CharSequence): Appendable = {
      inner.append(cs)

      if(cs.length() > 1) {
        prelast = cs.charAt(cs.length() - 2)
        last = cs.charAt(cs.length() - 1)
      } else if(cs.length() > 0) {
        prelast = last
        last = cs.charAt(cs.length() - 1)
      }
      this
    }

    override def append(cs: CharSequence, start: Int, end: Int): Appendable = {
      inner.append(cs, start, end)

      if(end - start > 1) {
        prelast = cs.charAt(end - 2)
        last = cs.charAt(end - 1)
      } else if(end - start > 0) {
        prelast = last
        last = cs.charAt(end - 1)
      }

      this
    }

    override def append(c: Char): Appendable = {
      inner.append(c)
      prelast = last
      last = c
      this
    }

    def miniContext: String =
      s"$prelast$last"
  }

  def newline(out: Appendable, indentation: Int): Unit = {
    out.append("\n")
    out.append("  " * indentation)
  }

  def write(out: Path, tree: Tree): Unit = {
    val result = TreeSyntax[Tree](Scala213)(tree)
    Using(Files.newBufferedWriter(out, StandardCharsets.UTF_8)) { writer =>
      write(writer, result)
    }
  }

  def write(out: Appendable, result: Result): Unit =
    write(new MiniContextAppendable(out), result, 0)

  /**
   * @see [[Result.toString]]
   */
  def write(out: MiniContextAppendable, result: Result, indentation: Int): Unit =
    result match {
      case None =>
      case Str(text) => out.append(text)
      case Sequence(xs@_*) => xs.foreach(write(out, _, indentation))
      case Repeat(first +: tail, sep) =>
        write(out, first, indentation)
        tail.foreach { x =>
          out.append(sep)
          write(out, x, indentation)
        }
      case Repeat(_, _) =>
      case Indent(res) =>
        newline(out, indentation + 1)
        write(out, res, indentation + 1)
      case Newline(res) =>
        newline(out, indentation)
        write(out, res, indentation)
      case Meta(_, res) =>
        write(out, res, indentation)
      case Wrap(prefix, res, suffix, cond) =>
        val s_res = res.toString
        if (cond(s_res)) out.append(prefix)
        out.append(s_res)
        if (cond(s_res)) out.append(suffix)
      case Function(fn) =>
        if(fn.getClass.getName.startsWith("scala.meta.internal.prettyprinters.TreeSyntax$SyntaxInstances$$Lambda$")) {
          // Currently the only instance: prepends a space if the surrounding output is dangerous.
          // Luckily it considers only the preceding two characters, so that's all the buffer we'll store
          val sb = new StringBuilder(out.miniContext)
          write(out, fn(sb), indentation)
        } else {
          System.err.println(fn.getClass.getName)
          ???
        }
    }
}
