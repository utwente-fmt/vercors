package vct.col.util

import hre.util.ScopedStack
import vct.col.ast.{AbstractRewriter, Declaration}
import vct.col.ref.Ref
import vct.col.util.Scopes._
import vct.result.Message
import vct.result.VerificationError.SystemError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try

object Scopes {
  case class WrongDeclarationCount(kind: ClassTag[_], count: Int)
      extends SystemError {
    override def text: String =
      messageContext(
        s"Expected exactly one declaration of kind ${kind.runtimeClass.getSimpleName}, but got $count."
      )
  }

  case class NoScope(kind: ClassTag[_]) extends SystemError {
    override def text: String =
      messageContext(
        s"There is no scope to place a declaration of kind ${kind.runtimeClass.getSimpleName} in."
      )
  }

  case class DuplicateSuccessor(
      pre: Declaration[_],
      firstPost: Declaration[_],
      secondPost: Declaration[_],
  ) extends SystemError {
    override def text: String =
      Message.messagesInContext(
        firstPost.highlight(firstPost) ->
          "This declaration already succeeds ...",
        context[CurrentProgramContext].map(_.program).getOrElse(pre)
          .highlight(pre) ->
          "... this declaration, but is additionally succeeded by ...",
        secondPost.highlight(secondPost) -> "... this declaration.",
      )
  }

  case class NoSuccessor(pre: Declaration[_]) extends SystemError {
    override def text: String = {
      Try {
        val useProgram = getContext[CurrentCheckProgramContext].program
        val useNode = getContext[CurrentCheckNodeContext].node
        val predProgram = getContext[CurrentRewriteProgramContext].program
        val predDecl = getContext[ConstructingSuccessorOfContext].decl
        Message.messagesInContext(
          (
            predProgram.highlight(predDecl),
            "A reference to the successor of this declaration was made, ...",
          ),
          (
            useProgram.highlight(useNode),
            "... but it has no successor in this position.",
          ),
        )
      } getOrElse {
        pre.o.messageInContext(
          "A reference to the successor of this declaration was made, but it has no successor."
        )
      }
    }
  }
}

case class Scopes[Pre, Post, PreDecl <: Declaration[
  Pre
], PostDecl <: Declaration[Post]]()(implicit tag: ClassTag[PostDecl]) {
  private val successors: ScopedStack[mutable.Map[PreDecl, PostDecl]] =
    ScopedStack()
  private val collectionBuffer: ScopedStack[mutable.ArrayBuffer[PostDecl]] =
    ScopedStack()

  def freeze: FrozenScopes[Pre, Post, PreDecl, PostDecl] =
    new FrozenScopes(successors.toSeq)

  def scope[T](f: => T): T = successors.having(mutable.Map())(f)

  def isEmpty: Boolean = collectionBuffer.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def collect[T](f: => T): (Seq[PostDecl], T) = {
    val buffer = ArrayBuffer[PostDecl]()

    val result = collectionBuffer.having(buffer)(f)

    (buffer.toSeq, result)
  }

  def collectScoped[T](f: => T): (Seq[PostDecl], T) = scope { collect(f) }

  def declare[T <: PostDecl](decl: T): T = {
    collectionBuffer.topOption match {
      case Some(buffer) => buffer += decl; decl
      case None => throw NoScope(tag)
    }
  }

  def succeedOnly[T <: PostDecl](pre: PreDecl, post: T)(
      implicit tag: ClassTag[T]
  ): T =
    successors.topOption match {
      case Some(map) if !map.contains(pre) => map(pre) = post; post
      case Some(map) => throw DuplicateSuccessor(pre, map(pre), post)
      case None => throw NoScope(tag)
    }

  def succeed[T <: PostDecl](pre: PreDecl, post: T)(
      implicit tag: ClassTag[T]
  ): T = {
    declare(post)
    succeedOnly(pre, post)
  }

  def dispatch(
      decl: PreDecl
  )(implicit rw: AbstractRewriter[Pre, Post]): PostDecl = {
    val result = collect { rw.dispatch(decl.asInstanceOf[Declaration[Pre]]) }
    result._1 match {
      case Seq(decl) => decl
      case other => throw WrongDeclarationCount(tag, other.size)
    }
  }

  def dispatch(
      decls: Iterable[PreDecl]
  )(implicit rw: AbstractRewriter[Pre, Post]): Seq[PostDecl] =
    collect {
      decls.foreach((decl: PreDecl) =>
        rw.dispatch(decl.asInstanceOf[Declaration[Pre]])
      )
    }._1

  def dispatch[PreRefDecl <: PreDecl, PostRefDecl <: PostDecl](
      ref: Ref[Pre, PreRefDecl]
  )(implicit tag: ClassTag[PostRefDecl]): Ref[Post, PostRefDecl] =
    freeze.succ(ref.decl)
}
