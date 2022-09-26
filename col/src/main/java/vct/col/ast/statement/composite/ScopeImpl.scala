package vct.col.ast.statement.composite

import vct.col.ast.Scope
import vct.col.check.CheckContext
import vct.col.resolve.ResolveReferences

trait ScopeImpl[G] { this: Scope[G] =>
  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    context.withScope((locals ++ ResolveReferences.scanScope(body, inGPUKernel = false)).toSet)
}