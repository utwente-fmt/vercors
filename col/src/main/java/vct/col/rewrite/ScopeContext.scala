package vct.col.rewrite

import hre.util.{FuncTools, ScopedStack}
import vct.col.ast._
import vct.col.ref.{LazyRef, Ref}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.SystemError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class ScopeContext[Pre, Post] {
  import ScopeContext._

  protected val successionMap: ScopedStack[SuccessionMap[Declaration[Pre], Declaration[Post]]] = ScopedStack()
  successionMap.push(SuccessionMap())

  // The default action for declarations is to be succeeded by a similar declaration, for example a copy.
  def succeed(predecessor: Declaration[Pre], successor: Declaration[Post]): Unit =
    successionMap.top(predecessor) = successor

  def freshSuccessionScope[T](f: => T): T = successionMap.having(SuccessionMap())(f)

  val globalScopes: ScopedStack[ArrayBuffer[GlobalDeclaration[Post]]] = ScopedStack()
  val classScopes: ScopedStack[ArrayBuffer[ClassDeclaration[Post]]] = ScopedStack()
  val adtScopes: ScopedStack[ArrayBuffer[ADTDeclaration[Post]]] = ScopedStack()
  val variableScopes: ScopedStack[ArrayBuffer[Variable[Post]]] = ScopedStack()
  val labelScopes: ScopedStack[ArrayBuffer[LabelDecl[Post]]] = ScopedStack()
  val sendScopes: ScopedStack[ArrayBuffer[SendDecl[Post]]] = ScopedStack()
  val parBlockScopes: ScopedStack[ArrayBuffer[ParBlockDecl[Post]]] = ScopedStack()
  val parInvariantScopes: ScopedStack[ArrayBuffer[ParInvariantDecl[Post]]] = ScopedStack()
  val modelScopes: ScopedStack[ArrayBuffer[ModelDeclaration[Post]]] = ScopedStack()

  val javaLocalScopes: ScopedStack[ArrayBuffer[JavaLocalDeclaration[Post]]] = ScopedStack()
  val javaParams: ScopedStack[ArrayBuffer[JavaParam[Post]]] = ScopedStack()

  val cLocalScopes: ScopedStack[ArrayBuffer[CDeclaration[Post]]] = ScopedStack()
  val cParams: ScopedStack[ArrayBuffer[CParam[Post]]] = ScopedStack()


  def withCollectInScope[T, S](scope: ScopedStack[ArrayBuffer[T]])(f: => S): (Seq[T], S) = {
    scope.push(ArrayBuffer())
    val s = f
    val ts = scope.pop().toSeq
    (ts, s)
  }

  def collectInScope[T](scope: ScopedStack[ArrayBuffer[T]])(f: => Unit): Seq[T] =
    withCollectInScope(scope)(f)._1

  def openScope[T](scope: ScopedStack[ArrayBuffer[T]]): Unit =
    scope.push(ArrayBuffer())

  def closeScope[T](scope: ScopedStack[ArrayBuffer[T]]): Seq[T] =
    scope.pop().toSeq

  def collectOneInScope[T](scope: ScopedStack[ArrayBuffer[T]])(f: => Unit)(implicit tag: ClassTag[T]): T = {
    val result = collectInScope(scope)(f)

    if (result.size != 1) {
      throw WrongDeclarationCount(tag, result.size)
    }

    result.head
  }

  def lookupSuccessor: Declaration[Pre] => Option[Declaration[Post]] = {
    val frozenSuccessionMap = successionMap.toSeq
    (decl: Declaration[Pre]) =>
      FuncTools.firstOption[SuccessionMap[Declaration[Pre], Declaration[Post]], Declaration[Post]](frozenSuccessionMap, _.get(decl))
  }

  def succ[DPost <: Declaration[Post]](ref: Ref[Pre, _ <: Declaration[Pre]])(implicit tag: ClassTag[DPost]): Ref[Post, DPost] =
    succ(ref.decl)

  def succ[DPost <: Declaration[Post]](decl: Declaration[Pre])(implicit tag: ClassTag[DPost]): Ref[Post, DPost] = {
    val fetcher = lookupSuccessor
    new LazyRef[Post, DPost](fetcher(decl).get)
  }

  def transmutePostRef
    [Decl[_] <: Declaration[_], DPre <: Declaration[Pre], DPost <: Declaration[Post]]
    (ref: Ref[Post, DPost])
    (implicit w1: DPost <:< Decl[Post], w2: Decl[Pre] <:< DPre, tag: ClassTag[DPre])
    : Ref[Pre, DPre] = {
    successionMap.top((ref.decl : Declaration[Post]).asInstanceOf[Declaration[Pre]]) = ref.decl
    Ref.transmute[Post, Pre, Decl, DPost, DPre](ref)
  }
}

object ScopeContext {
  case class WrongDeclarationCount(kind: ClassTag[_], count: Int) extends SystemError {
    override def text: String =
      s"Expected exactly one declaration of kind ${kind.runtimeClass.getSimpleName}, but got $count."
  }
}