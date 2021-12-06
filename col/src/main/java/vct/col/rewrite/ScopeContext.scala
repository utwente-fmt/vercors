package vct.col.rewrite

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

  val globalScopes: mutable.Stack[ArrayBuffer[GlobalDeclaration]] = mutable.Stack()
  val classScopes: mutable.Stack[ArrayBuffer[ClassDeclaration]] = mutable.Stack()
  val adtScopes: mutable.Stack[ArrayBuffer[ADTDeclaration]] = mutable.Stack()
  val variableScopes: mutable.Stack[ArrayBuffer[Variable]] = mutable.Stack()
  val labelScopes: mutable.Stack[ArrayBuffer[LabelDecl]] = mutable.Stack()
  val parBlockScopes: mutable.Stack[ArrayBuffer[ParBlockDecl]] = mutable.Stack()
  val parInvariantScopes: mutable.Stack[ArrayBuffer[ParInvariantDecl]] = mutable.Stack()
  val modelScopes: mutable.Stack[ArrayBuffer[ModelDeclaration]] = mutable.Stack()

  val javaLocalScopes: mutable.Stack[ArrayBuffer[JavaLocalDeclaration]] = mutable.Stack()
  val cLocalScopes: mutable.Stack[ArrayBuffer[CDeclaration]] = mutable.Stack()
  val cParams: mutable.Stack[ArrayBuffer[CParam]] = mutable.Stack()

  def collectInScope[T](scope: mutable.Stack[ArrayBuffer[T]])(f: => Unit): Seq[T] = {
    scope.push(ArrayBuffer())
    f
    scope.pop().toSeq
  }

  def collectOneInScope[T](scope: mutable.Stack[ArrayBuffer[T]])(f: => Unit)(implicit tag: ClassTag[T]): T = {
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