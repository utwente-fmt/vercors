package vct.col.util

import hre.util.ScopedStack
import vct.col.ast.{AbstractRewriter, Declaration}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.Generation
import vct.col.util.Scopes._
import vct.result.VerificationError.SystemError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object Scopes {
  case class WrongDeclarationCount(kind: ClassTag[_], count: Int) extends SystemError {
    override def text: String =
      s"Expected exactly one declaration of kind ${kind.runtimeClass.getSimpleName}, but got $count."
  }

  case class NoScope(kind: ClassTag[_]) extends SystemError {
    override def text: String =
      s"There is no scope to place a declaration of kind ${kind.runtimeClass.getSimpleName} in."
  }
}

case class Scopes[Pre <: Generation, Post <: Generation, PreDecl <: Declaration[Pre], PostDecl <: Declaration[Post]](rw: AbstractRewriter[Pre, Post])(implicit tag: ClassTag[PostDecl]) {
  private val successors: ScopedStack[mutable.Map[PreDecl, PostDecl]] = ScopedStack()
  private val collectionBuffer: ScopedStack[mutable.ArrayBuffer[PostDecl]] = ScopedStack()

  /**
   * When constructing a lazy reference, it is important that the stack of scopes is recorded *before* we enter a lazy
   * context. For example, this is wrong:
   *
   * new LazyRef(successors.toSeq.collectFirst(_(decl)).get)
   *
   * since everything between the LazyRef parentheses is evaluated lazily, and only the value of successors is captured
   * in the closure.
   */
  class FrozenScopes {
    val scopes: Seq[mutable.Map[PreDecl, PostDecl]] = successors.toSeq

    def succ[RefDecl <: PostDecl](decl: PreDecl)(implicit tag: ClassTag[RefDecl]): Ref[Post, RefDecl] =
      new LazyRef(scopes.collectFirst(_(decl)).get)
  }

  def freeze: FrozenScopes = new FrozenScopes

  def scope[T](f: => T): T =
    successors.having(mutable.Map())(f)

  def collect[T](f: => T): (Seq[PostDecl], T) = {
    val buffer = ArrayBuffer[PostDecl]()

    val result = collectionBuffer.having(buffer) {
      f
    }

    (buffer.toSeq, result)
  }

  def collectScoped[T](f: => T): (Seq[PostDecl], T) =
    scope { collect(f) }

  def declare[T <: PostDecl](decl: T): T = {
    collectionBuffer.topOption match {
      case Some(buffer) => buffer += decl; decl
      case None => throw NoScope(tag)
    }
  }

  def dispatch(decl: PreDecl): PostDecl = {
    val result = collect { rw.dispatch(decl.asInstanceOf[Declaration[Pre]]) }
    result._1 match {
      case Seq(decl) => decl
      case other => throw WrongDeclarationCount(tag, other.size)
    }
  }

  def dispatch(decls: Seq[PreDecl]): Seq[PostDecl] =
    collect { decls.foreach((decl: PreDecl) => rw.dispatch(decl.asInstanceOf[Declaration[Pre]])) }._1

  def dispatch[PreRefDecl <: PreDecl, PostRefDecl <: PostDecl](ref: Ref[Pre, PreRefDecl])(implicit tag: ClassTag[PostRefDecl]): Ref[Post, PostRefDecl] =
    freeze.succ(ref.decl)
}
