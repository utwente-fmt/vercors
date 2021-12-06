package vct.col.newrewrite

import vct.col.ast.{Block, LocalDecl, Statement}
import vct.col.rewrite.Rewriter
import vct.col.ast.RewriteHelpers._
import vct.result.VerificationResult.Unreachable

case class CollectLocalDeclarations() extends Rewriter {
  override def dispatch(stat: Statement): Statement = stat match {
    case LocalDecl(local) =>
      if(variableScopes.isEmpty) {
        throw Unreachable("The frontend must not open an execution context (e.g. method body) that does not immediately contain a scope.")
      }

      local.rewrite().succeedDefault(this, local)
      Block(Nil)(stat.o)
    case other => rewriteDefault(other)
  }
}