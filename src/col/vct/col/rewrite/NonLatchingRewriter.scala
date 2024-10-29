package vct.col.rewrite

import vct.col.ast._
import vct.col.ast.rewrite.BaseNonLatchingRewriter
import vct.col.util.CurrentRewriteProgramContext
import vct.result.VerificationError

class NonLatchingRewriter[Pre, Post]()
    extends BaseNonLatchingRewriter[Pre, Post] {
  override def dispatch(program: Program[Pre]): Program[Post] =
    VerificationError.withContext(CurrentRewriteProgramContext(program)) {
      program.rewriteDefault()
    }

  override def dispatch(decl: Declaration[Pre]): Unit =
    allScopes.anySucceed(decl, decl.rewriteDefault())
}
