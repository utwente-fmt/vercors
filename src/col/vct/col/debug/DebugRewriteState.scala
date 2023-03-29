package vct.col.debug

sealed trait DebugRewriteState
case object NotProcessed extends DebugRewriteState
case object Succeeded extends DebugRewriteState
case object Dropped extends DebugRewriteState