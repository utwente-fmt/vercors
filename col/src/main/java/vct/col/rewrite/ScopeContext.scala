package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ref.{LazyRef, Ref}
import vct.col.util.SuccessionMap
import vct.result.VerificationResult.SystemError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class ScopeContext {

  import ScopeContext._

  // The default action for declarations is to be succeeded by a similar declaration, for example a copy.
  val successionMap: SuccessionMap[Declaration, Declaration] = SuccessionMap()

  val globalScopes: ScopedStack[ArrayBuffer[GlobalDeclaration]] = ScopedStack()
  val classScopes: ScopedStack[ArrayBuffer[ClassDeclaration]] = ScopedStack()
  val adtScopes: ScopedStack[ArrayBuffer[ADTDeclaration]] = ScopedStack()
  val variableScopes: ScopedStack[ArrayBuffer[Variable]] = ScopedStack()
  val labelScopes: ScopedStack[ArrayBuffer[LabelDecl]] = ScopedStack()
  val parBlockScopes: ScopedStack[ArrayBuffer[ParBlockDecl]] = ScopedStack()
  val parInvariantScopes: ScopedStack[ArrayBuffer[ParInvariantDecl]] = ScopedStack()
  val modelScopes: ScopedStack[ArrayBuffer[ModelDeclaration]] = ScopedStack()

  val javaLocalScopes: ScopedStack[ArrayBuffer[JavaLocalDeclaration]] = ScopedStack()
  val cLocalScopes: ScopedStack[ArrayBuffer[CDeclaration]] = ScopedStack()
  val cParams: ScopedStack[ArrayBuffer[CParam]] = ScopedStack()

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

  def succ[T <: Declaration](ref: Ref[T])(implicit tag: ClassTag[T]): Ref[T] =
    succ(ref.decl)

  def succ[T <: Declaration](decl: Declaration)(implicit tag: ClassTag[T]): Ref[T] =
    successionMap.ref(decl)
}

object ScopeContext {
  case class WrongDeclarationCount(kind: ClassTag[_], count: Int) extends SystemError {
    override def text: String =
      s"Expected exactly one declaration of kind ${kind.runtimeClass.getSimpleName}, but got $count."
  }
}