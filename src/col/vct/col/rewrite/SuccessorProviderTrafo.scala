package vct.col.rewrite

import vct.col.ast.Declaration

class SuccessorProviderTrafo[Pre, Post, PreDecl <: Declaration[
  Pre
], PostDecl <: Declaration[Post]](
    inner: SuccessorProvider[Pre, Post, PreDecl, PostDecl]
) extends SuccessorProvider[Pre, Post, PreDecl, PostDecl] {
  def preTransform[I <: PreDecl, O <: PostDecl](pre: I): Option[O] = None
  def postTransform[T <: PostDecl](pre: PreDecl, post: Option[T]): Option[T] =
    post

  override def computeSucc(decl: PreDecl): Option[PostDecl] =
    preTransform(decl).orElse(postTransform(decl, inner.computeSucc(decl)))

}
