package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ref.{LazyRef, Ref}
import vct.col.util.SuccessionMap
import vct.result.VerificationResult.SystemError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class ScopeContext[Pre, Post] {

  import ScopeContext._

  // The default action for declarations is to be succeeded by a similar declaration, for example a copy.
  val successionMap: SuccessionMap[Declaration[Pre], Declaration[Post]] = SuccessionMap()

  val globalScopes: ScopedStack[ArrayBuffer[GlobalDeclaration[Post]]] = ScopedStack()
  val classScopes: ScopedStack[ArrayBuffer[ClassDeclaration[Post]]] = ScopedStack()
  val adtScopes: ScopedStack[ArrayBuffer[ADTDeclaration[Post]]] = ScopedStack()
  val variableScopes: ScopedStack[ArrayBuffer[Variable[Post]]] = ScopedStack()
  val labelScopes: ScopedStack[ArrayBuffer[LabelDecl[Post]]] = ScopedStack()
  val parBlockScopes: ScopedStack[ArrayBuffer[ParBlockDecl[Post]]] = ScopedStack()
  val parInvariantScopes: ScopedStack[ArrayBuffer[ParInvariantDecl[Post]]] = ScopedStack()
  val modelScopes: ScopedStack[ArrayBuffer[ModelDeclaration[Post]]] = ScopedStack()

  val javaLocalScopes: ScopedStack[ArrayBuffer[JavaLocalDeclaration[Post]]] = ScopedStack()
  val cLocalScopes: ScopedStack[ArrayBuffer[CDeclaration[Post]]] = ScopedStack()
  val cParams: ScopedStack[ArrayBuffer[CParam[Post]]] = ScopedStack()

  def collectInScope[T](scope: ScopedStack[ArrayBuffer[T]])(f: => Unit): Seq[T] = {
    scope.push(ArrayBuffer())
    f
    scope.pop().toSeq
  }

  def collectOneInScope[T](scope: ScopedStack[ArrayBuffer[T]])(f: => Unit)(implicit tag: ClassTag[T]): T = {
    val result = collectInScope(scope)(f)

    if (result.size != 1) {
      throw WrongDeclarationCount(tag, result.size)
    }

    result.head
  }

  def succ[DPre <: Declaration[Pre], DPost <: Declaration[Post]](ref: Ref[Pre, DPre])(implicit tag: ClassTag[DPost]): Ref[Post, DPost] =
    succ(ref.decl)

  def succ[DPre <: Declaration[Pre], DPost <: Declaration[Post]](decl: DPre)(implicit tag: ClassTag[DPost]): Ref[Post, DPost] =
    successionMap.ref(decl)

  def transmutePostRef
    [Decl[_] <: Declaration[_], DPre <: Declaration[Pre], DPost <: Declaration[Post]]
    (ref: Ref[Post, DPost])
    (implicit w1: DPost <:< Decl[Post], w2: Decl[Pre] <:< DPre, tag: ClassTag[DPre])
    : Ref[Pre, DPre] = {
    successionMap((ref.decl : Declaration[Post]).asInstanceOf[Declaration[Pre]]) = ref.decl
    Ref.transmute[Post, Pre, Decl, DPost, DPre](ref)
  }
}

object ScopeContext {
  case class WrongDeclarationCount(kind: ClassTag[_], count: Int) extends SystemError {
    override def text: String =
      s"Expected exactly one declaration of kind ${kind.runtimeClass.getSimpleName}, but got $count."
  }
}