package vct.col.ast.node

import vct.col.ast.Node

import scala.collection.SeqOps

trait NodeSubnodeOps[G] {
  this: Node[G] =>

  def visit(f: Node[G] => Unit): Unit = {
    f(this)
    subnodes.foreach(_.visit(f))
  }

  def foreach(f: Node[G] => Unit): Unit = visit(f)

  def collect[T](f: PartialFunction[Node[G], T]): Seq[T] = {
    val builder = Vector.newBuilder[T]
    val optF = f.lift
    visit { node =>
      optF(node) match {
        case None =>
        case Some(t) => builder += t
      }
    }
    builder.result()
  }

  def flatCollect[T](f: PartialFunction[Node[G], IterableOnce[T]]): Seq[T] = {
    val builder = IndexedSeq.newBuilder[T]
    val optF = f.lift
    visit { node =>
      optF(node) match {
        case None =>
        case Some(ts) => builder ++= ts
      }
    }
    builder.result()
  }

  def collectFirst[T](f: PartialFunction[Node[G], T]): Option[T] = {
    val optF = f.lift
    visit { node =>
      optF(node) match {
        case None =>
        case Some(t) => return Some(t)
      }
    }
    None
  }

  def exists[T](f: PartialFunction[Node[G], Boolean]): Boolean = {
    var res = false
    visit(node => res = res || f.lift(node).getOrElse(false))
    res
  }

  def count[T](f: PartialFunction[Node[G], Unit]): Int = {
    var result: Int = 0
    val optF = f.lift
    visit(node =>
      if (optF(node).nonEmpty)
        result += 1
    )
    result
  }

}
