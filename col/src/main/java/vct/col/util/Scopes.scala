package vct.col.util

import hre.util.ScopedStack
import vct.col.ast.{AbstractRewriter, Declaration}
import vct.col.ref.Ref
import vct.col.rewrite.Generation
import vct.col.util.Scopes.WrongDeclarationCount
import vct.result.VerificationError.SystemError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object Scopes {
  case class WrongDeclarationCount(kind: ClassTag[_], count: Int) extends SystemError {
    override def text: String =
      s"Expected exactly one declaration of kind ${kind.runtimeClass.getSimpleName}, but got $count."
  }
}

case class Scopes[Pre <: Generation, Post <: Generation, Decl[_] <: Declaration[_]](rw: AbstractRewriter[Pre, Post])(implicit tag: ClassTag[Decl[_]]) {
  val successors: ScopedStack[mutable.Map[Decl[Pre], Decl[Post]]] = ScopedStack()
  val collectionBuffer: ScopedStack[mutable.ArrayBuffer[Decl[Post]]] = ScopedStack()

  def scope[T](f: => T): T =
    try {
      successors.push(mutable.Map())
      f
    } finally {
      successors.pop()
    }

  def collect[T](f: => T): (Seq[Decl[Post]], T) = {
    val buffer = ArrayBuffer[Decl[Post]]()

    val result = collectionBuffer.having(buffer) {
      f
    }

    (buffer.toSeq, result)
  }

  def collectScoped[T](f: => T): (Seq[Decl[Post]], T) =
    scope { collect(f) }

  def dispatch(decl: Decl[Pre]): Decl[Post] = {
    val result = collect { rw.dispatch(decl.asInstanceOf[Declaration[Pre]]) }
    result._1 match {
      case Seq(decl) => decl
      case other => throw WrongDeclarationCount(tag, other.size)
    }
  }

  def dispatch(decls: Seq[Decl[Pre]]): Seq[Decl[Post]] =
    collect { decls.foreach((decl: Decl[Pre]) => rw.dispatch(decl.asInstanceOf[Declaration[Pre]])) }._1

  def succ(decl: Decl[Pre]): Ref[Post, Decl[Post]] = {
    val frozenSuccessors = successors.toSeq
    ???
  }
}
