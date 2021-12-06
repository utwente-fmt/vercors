package vct.col.ast.temporaryimplpackage.statement.composite

import vct.col.ast.Scope
import vct.col.check.CheckContext
import vct.col.resolve.ResolveReferences

trait ScopeImpl { this: Scope =>
  override def enterCheckContext(context: CheckContext): CheckContext = context.withScope((locals ++ ResolveReferences.scanScope(body)).toSet)
}