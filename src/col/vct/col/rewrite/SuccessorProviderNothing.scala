package vct.col.rewrite

import vct.col.ast.Declaration

class SuccessorProviderNothing[Pre, Post, PreDecl <: Declaration[
  Pre
], PostDecl <: Declaration[Post]](crash: => Nothing)
    extends SuccessorProvider[Pre, Post, PreDecl, PostDecl] {
  override def computeSucc(decl: PreDecl): Nothing = crash
}
