package vct.col.print

import vct.col.ast.Node
import vct.col.origin.InputOrigin.LINE_NUMBER_WIDTH

import java.lang
import scala.annotation.tailrec

object Show {
  def lazily(f: Ctx => Doc): Show = new Show {
    override def show(implicit ctx: Ctx): Doc = f(ctx)
  }
}

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
    docs.map(_.show).filter(_.nonEmpty).reduceLeftOption(f).getOrElse(Empty)

  def spread(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    fold(docs)(_ <+> _)

  def rspread(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    fold(docs.map(_.show <+> Empty))(_ <> _)

  def lspread(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    fold(docs.map(Empty <+> _.show))(_ <> _)

  def stack(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    fold(docs)(_ <+/> _)

  def arg(doc: Show)(implicit ctx: Ctx): Doc =
    Nest(NonWsLine <> doc) <> NonWsLine

  def args(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    if(docs.nonEmpty) arg(fold(docs)(_ <> "," <+/> _))
    else Empty

  def inlineSpec(doc: Show)(implicit ctx: Ctx): Doc =
    if(ctx.syntax == Ctx.PVL) doc.show
    else if(ctx.inSpec) doc.show
    else {
      val d = doc.show(ctx.copy(inSpec = true))
      if(d.nonEmpty) Text("/*@") <+> d <+> "@*/"
      else Empty
    }

  def spec(doc: Show)(implicit ctx: Ctx): Doc =
    if (ctx.syntax == Ctx.PVL) doc.show
    else if (ctx.inSpec) doc.show
    else {
      val d = doc.show(ctx.copy(inSpec = true))
      if (d.nonEmpty) Text("/*@") <+/> d <+/> "@*/"
      else Empty
    }
}

/**
  * This is an implementation of A prettier printer by Philip Wadler, accessible here:
  * https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
  *
  * We've made a couple modifications to suit our purposes:
  *
  * (1) There is a newline that is not flattened into a space, but into no text, to support our layout for invocations
  *     that are split over lines:
  *     e.g.:
  *
  *     someMethod(
  *         1,
  *         2,
  *         3
  *     )
  *
  *     would otherwise be flattened into someMethod( 1, 2, 3 ), so the initial and last newline are non-whitespace
  *     newlines:
  *
  *     someMethod(`</>`
  *       1,`<+/>`
  *       2,`<+/>`
  *       3,`</>`
  *     )
  * (2) Group is an explicit data structure, as defined: Group(x) = flatten x <|> x. Note that contrary to the canonical
  *     definition, <+/> does not introduce an alternative - it is typically grouped in a larger expression, to prefer
  *     splitting larger trees over lines.
  * (3) be includes additional state to track whether an element needs to be flattened.
  * (4) NodeDoc associates a node in the AST with a Doc in the rendered tree, for additional post-processing of the
  *     layout.
  */
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
      case EStart(_) =>
      case EEnd(_) =>
    }
  }
  case class EText(text: String) extends Elem
  case class ELine(indent: Int) extends Elem
  case class EStart(node: Node[_]) extends Elem
  case class EEnd(node: Node[_]) extends Elem


  def lazyListLen(list: LazyList[Elem]): Int = list match {
    case LazyList() => 0
    case EText(text) #:: tail => text.length + lazyListLen(tail)
    case EStart(_) #:: tail => lazyListLen(tail)
    case EEnd(_) #:: tail => lazyListLen(tail)
    case ELine(indent) #:: tail => 1 + indent + lazyListLen(tail)
  }

  @tailrec
  private def fits(spent: Int, elems: LazyList[Elem])(implicit ctx: Ctx): Boolean = elems match {
    case _ if spent > ctx.width => false
    case LazyList() => true
    case EText(t) #:: elems => fits(spent + t.length, elems)
    case ELine(_) #:: _ => true
    case EStart(_) #:: tail => fits(spent, tail)
    case EEnd(_) #:: tail => fits(spent, tail)
  }

  private def better(spent: Int, x: LazyList[Elem], y: LazyList[Elem])(implicit ctx: Ctx): LazyList[Elem] =
    if(fits(spent, x)) x else y

  private def be(spent: Int, docs: Seq[(Int, Boolean, Seq[Node[_]], Doc)])(implicit ctx: Ctx): LazyList[Elem] = docs match {
    case Nil => LazyList.empty
    case (_, _, nes, Empty) +: docs => nes.map(EEnd).to(LazyList) #::: be(spent, docs)
    case (i, f, nes, NodeDoc(node, x)) +: docs => EStart(node) #:: be(spent, (i, f, nes :+ node, x) +: docs)
    case (i, f, nes, Cons(x, y)) +: docs => be(spent, (i, f, Nil, x) +: (i, f, nes, y) +: docs)
    case (i, f, nes, Nest(x)) +: docs => be(spent, (i+ctx.tabWidth, f, nes, x) +: docs)
    case (_, _, nes, Text(t)) +: docs => EText(t) #:: nes.map(EEnd).to(LazyList) #::: be(spent + t.length, docs)
    case (i, false, nes, Line | NonWsLine) +: docs => ELine(i) #:: nes.map(EEnd).to(LazyList) #::: be(i, docs)
    case (_, true, nes, Line) +: docs => EText(" ") #:: nes.map(EEnd).to(LazyList) #::: be(spent + 1, docs)
    case (_, true, nes, NonWsLine) +: docs => nes.map(EEnd).to(LazyList) #::: be(spent, docs)
    case (i, true, nes, Group(x)) +: docs => be(spent, (i, true, nes, x) +: docs)
    case (i, false, nes, Group(x)) +: docs => better(
      spent,
      be(spent, (i, true, nes, x) +: docs),
      be(spent, (i, false, nes, x) +: docs),
    )
  }

  def pretty(implicit ctx: Ctx): LazyList[Elem] =
    be(0, Seq((0, false, Nil, this)))

  def nonEmpty: Boolean = this match {
    case Empty => false
    case NonWsLine => true
    case Line => true
    case Cons(left, right) => left.nonEmpty || right.nonEmpty
    case Text(text) => text.nonEmpty
    case Nest(doc) => doc.nonEmpty
    case Group(doc) => doc.nonEmpty
    case NodeDoc(_, doc) => doc.nonEmpty
  }

  def splitOn[A](list: LazyList[A])(predicate: A => Boolean): LazyList[LazyList[A]] = {
    def loop(l: LazyList[A], acc: LazyList[A]): LazyList[LazyList[A]] = l match {
      case LazyList() => LazyList(acc)
      case h #:: t if predicate(h) => acc #:: loop(t, LazyList(h))
      case h #:: t => loop(t, acc :+ h)
    }
    loop(list, LazyList())
  }

  def lines(implicit ctx: Ctx): LazyList[LazyList[Elem]] =
    splitOn(pretty(ctx)) {
      case ELine(_) => true
      case _ => false
    }.map(_.map {
      case ELine(i) => EText(" ".repeat(i))
      case other => other
    })



  def highlight(node: Node[_])(implicit ctx: Ctx): String = {
    val lineNumber = (line: Int) => String.format("%" + f"$LINE_NUMBER_WIDTH" + "d ", line)

    val sb = new lang.StringBuilder()
    val (prefix, rest)= lines.zipWithIndex.span(!_._1.contains(EStart(node)))
    val (highlightInit, rest1) = rest.span(!_._1.contains(EEnd(node)))
    if (rest1.isEmpty) {
      sb.append("⋱\n")
      node.show(ctx.namesIn(node)).lines.foreach { line =>
        sb.append("  ")
        line.foreach(_.write(sb))
      }
      sb.append("\n⋰")
      return sb.toString
    }
    val highlight = highlightInit :+ rest1.head
    val suffix = rest1.tail
    prefix.takeRight(2).foreach { lineWithIndex =>
      sb.append(lineNumber(lineWithIndex._2 + 1))
      lineWithIndex._1.foreach(_.write(sb))
      sb.append("\n")
    }
    val indicatorStart = lazyListLen(highlight.map(_._1).head.takeWhile(_ != EStart(node)))
    val indicatorEnd = lazyListLen(highlight.map(_._1).head.takeWhile(_ != EEnd(node)))
    sb.append(" ".repeat(indicatorStart + LINE_NUMBER_WIDTH)).append("[").append("-".repeat(indicatorEnd-indicatorStart))
    highlight.foreach { lineWithIndex =>
      sb.append("\n")
      sb.append(lineNumber(lineWithIndex._2 + 1))
      lineWithIndex._1.foreach(_.write(sb))
    }
    val endIndicatorStart = if (highlight.size == 1) indicatorStart else 0
    val endIndicatorEnd = lazyListLen(highlight.map(_._1).last.takeWhile(_ != EEnd(node)))
    sb.append("\n")
    sb.append(" ".repeat(endIndicatorStart + LINE_NUMBER_WIDTH + 1)).append("-".repeat(endIndicatorEnd - endIndicatorStart)).append("]")
    suffix.take(2).foreach { lineWithIndex =>
      sb.append("\n")
      sb.append(lineNumber(lineWithIndex._2 + 1))
      lineWithIndex._1.foreach(_.write(sb))
    }
    sb.toString
  }
}

case object Empty extends Doc
case object NonWsLine extends Doc
case object Line extends Doc
case class Cons(left: Doc, right: Doc) extends Doc
case class Text(text: String) extends Doc
case class Nest(doc: Doc) extends Doc
case class Group(doc: Doc) extends Doc

case class NodeDoc(node: Node[_], doc: Doc) extends Doc
