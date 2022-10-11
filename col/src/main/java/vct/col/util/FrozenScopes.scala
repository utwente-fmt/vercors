package vct.col.util

import vct.col.ast.Declaration
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.SuccessorProvider

import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * When constructing a lazy reference, it is important that the stack of scopes is recorded *before* we enter a lazy
 * context. For example, this is wrong:
 *
 * new LazyRef(successors.toSeq.collectFirst(_(decl)).get)
 *
 * since everything between the LazyRef parentheses is evaluated lazily, and only the value of successors is captured
 * in the closure.
 */
class FrozenScopes[Pre, Post, PreDecl <: Declaration[Pre], PostDecl <: Declaration[Post]](val scopes: Seq[mutable.Map[PreDecl, PostDecl]]) extends SuccessorProvider[Pre, Post, PreDecl, PostDecl] {
  override def computeSucc(decl: PreDecl): Option[PostDecl] = scopes.collectFirst { case m if m.contains(decl) => m(decl) }

  override def succ[RefDecl <: Declaration[Post]](decl: PreDecl)(implicit tag: ClassTag[RefDecl]): Ref[Post, RefDecl] =
    new LazyRef(computeSucc(decl).get, Some(EqualityMeasure(this, decl)))
}