package vct.col.rewrite

import vct.col.ast.Declaration
import vct.col.ref.{LazyRef, MistypedRef, Ref}

import scala.reflect.ClassTag

trait SuccessorProvider[Pre, Post, PreDecl <: Declaration[Pre], PostDecl <: Declaration[Post]] {
  def computeSucc(decl: PreDecl): Option[PostDecl]

  def succ[RefDecl <: Declaration[Post]](decl: PreDecl)(implicit tag: ClassTag[RefDecl]): Ref[Post, RefDecl] =
    new LazyRef(computeSucc(decl).get)
}