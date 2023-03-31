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
  def fold(docs: Iterable[Show])(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc =
    docs.map(_.show).reduceLeftOption(f).getOrElse(Empty)

  def spread(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    fold(docs)(_ <+> _)

  def rspread(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    fold(docs.map(_.show <> " "))(_ <> _)

  def stack(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    fold(docs)(_ <+/> _)

  def args(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    if(docs.nonEmpty) Nest(NonWsLine <> fold(docs)(_ <> "," <+/> _)) <> NonWsLine
    else Empty
}

sealed trait Doc extends Show {
  def show(implicit ctx: Ctx): Doc = this

  def <>(other: Show)(implicit ctx: Ctx): Doc = Cons(this, other.show)
  def <>(other: String)(implicit ctx: Ctx): Doc = this <> Text(other)
  def <+>(other: Show)(implicit ctx: Ctx): Doc = this <> " " <> other.show
  def <+>(other: String)(implicit ctx: Ctx): Doc = this <+> Text(other)
  def </>(other: Show)(implicit ctx: Ctx): Doc = this <> NonWsLine <> other.show
  def </>(other: String)(implicit ctx: Ctx): Doc = this </> Text(other)
  def <+/>(other: Show)(implicit ctx: Ctx): Doc = this <> Line <> other.show
  def <+/>(other: String)(implicit ctx: Ctx): Doc = this <+/> Text(other)
  def <>>(other: Show)(implicit ctx: Ctx): Doc = this <> Nest(Line <> other)
  def <>>(other: String)(implicit ctx: Ctx): Doc = this <>> Text(other)

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

  private def be(spent: Int, docs: Seq[(Int, Boolean, Doc)])(implicit ctx: Ctx): LazyList[Elem] = docs match {
    case Nil => LazyList.empty
    case (_, _, Empty) :: docs => be(spent, docs)
    case (i, f, NodeDoc(_, x)) :: docs => be(spent, (i, f, x) +: docs)
    case (i, f, Cons(x, y)) :: docs => be(spent, (i, f, x) +: (i, f, y) +: docs)
    case (i, f, Nest(x)) :: docs => be(spent, (i+ctx.tabWidth, f, x) +: docs)
    case (_, _, Text(t)) :: docs => EText(t) #:: be(spent + t.length, docs)
    case (i, false, Line | NonWsLine) :: docs => ELine(i) #:: be(i, docs)
    case (_, true, Line) :: docs => EText(" ") #:: be(spent + 1, docs)
    case (_, true, NonWsLine) :: docs => be(spent, docs)
    case (i, true, Group(x)) :: docs => be(spent, (i, true, x) +: docs)
    case (i, false, Group(x)) :: docs => better(
      spent,
      be(spent, (i, true, x) +: docs),
      be(spent, (i, false, x) +: docs),
    )
  }

  def pretty(implicit ctx: Ctx): LazyList[Elem] =
    be(0, Seq((0, false, this)))
}

case object Empty extends Doc
case object NonWsLine extends Doc
case object Line extends Doc
case class Cons(left: Doc, right: Doc) extends Doc
case class Text(text: String) extends Doc
case class Nest(doc: Doc) extends Doc
case class Group(doc: Doc) extends Doc

case class NodeDoc(node: Node[_], doc: Doc) extends Doc
