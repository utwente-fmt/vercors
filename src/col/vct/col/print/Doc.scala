package vct.col.print

import vct.col.ast.Node

import java.lang
import scala.annotation.tailrec

trait Show {
  def show(implicit ctx: Ctx): Doc

  def write(a: Appendable)(implicit ctx: Ctx): Unit =
    show.pretty.foreach(_.write(a))

  def toStringWithContext(implicit ctx: Ctx): String = {
    val sb = new lang.StringBuilder()
    write(sb)
    sb.toString
  }
}

case object Doc {
  def fold(docs: Seq[Show])(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc =
    docs.map(_.show).reduceLeftOption(f).getOrElse(Empty)

  def spread(docs: Seq[Show])(implicit ctx: Ctx): Doc =
    fold(docs)(_ <+> _)

  def stack(docs: Seq[Show])(implicit ctx: Ctx): Doc =
    fold(docs)(_ </> _)

  def args(docs: Seq[Show])(implicit ctx: Ctx): Doc =
    if(docs.nonEmpty) Nest(NonWsLine <> fold(docs)(_ <> "," </> _)) <> NonWsLine
    else Empty
}

sealed trait Doc extends Show {
  def show(implicit ctx: Ctx): Doc = this

  def <>(other: Show)(implicit ctx: Ctx): Doc = Cons(this, other.show)
  def <>(other: String)(implicit ctx: Ctx): Doc = this <> Text(other)
  def <+>(other: Show)(implicit ctx: Ctx): Doc = this <> " " <> other.show
  def </>(other: Show)(implicit ctx: Ctx): Doc = this <> Line <> other.show

  sealed trait Elem {
    def write(a: Appendable): Unit = this match {
      case EText(text) => a.append(text)
      case ELine(indent) => a.append("\n").append(" ".repeat(indent))
    }
  }
  case class EText(text: String) extends Elem
  case class ELine(indent: Int) extends Elem

  @tailrec
  private def fits(spent: Int, elems: LazyList[Elem])(implicit ctx: Ctx): Boolean = elems match {
    case _ if spent > ctx.width => false
    case LazyList() => true
    case EText(t) #:: elems => fits(spent + t.length, elems)
    case ELine(_) #:: _ => true
  }

  private def better(spent: Int, x: LazyList[Elem], y: LazyList[Elem])(implicit ctx: Ctx): LazyList[Elem] =
    if(fits(spent, x)) x else y

  private def be(spent: Int, flatten: Boolean, docs: Seq[(Int, Doc)])(implicit ctx: Ctx): LazyList[Elem] = docs match {
    case Nil => LazyList.empty
    case (_, Empty) :: docs => be(spent, flatten, docs)
    case (i, NodeDoc(_, x)) :: docs => be(spent, flatten, (i, x) +: docs)
    case (i, Cons(x, y)) :: docs => be(spent, flatten, (i, x) +: (i, y) +: docs)
    case (i, Nest(x)) :: docs => be(spent, flatten, (i+ctx.tabWidth, x) +: docs)
    case (_, Text(t)) :: docs => EText(t) #:: be(spent + t.length, flatten, docs)
    case (i, Line | NonWsLine) :: docs if !flatten => ELine(i) #:: be(i, flatten, docs)
    case (_, Line) :: docs if flatten => EText(" ") #:: be(spent + 1, flatten, docs)
    case (_, NonWsLine) :: docs if flatten => be(spent, flatten, docs)
    case (i, Group(x)) :: docs => better(
      spent,
      be(spent, flatten = true, (i, x) +: docs),
      be(spent, flatten = flatten, (i, x) +: docs),
    )
  }

  def pretty(implicit ctx: Ctx): LazyList[Elem] =
    be(0, flatten = false, Seq((0, this)))
}

case object Empty extends Doc
case object NonWsLine extends Doc
case object Line extends Doc
case class Cons(left: Doc, right: Doc) extends Doc
case class Text(text: String) extends Doc
case class Nest(doc: Doc) extends Doc
case class Group(doc: Doc) extends Doc

case class NodeDoc(node: Node[_], doc: Doc) extends Doc
