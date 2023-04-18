package vct.col.rewrite

import vct.col.ast.Deserialize
import vct.col.origin.{LLVMOrigin, Origin}

case object DeserializeLLVMOrigin extends RewriterBuilder {
  override def key: String = "llvmOrigin"

  override def desc: String = "Rewrites all Deserialize.Origin to LLVMOrigin"
}

case class DeserializeLLVMOrigin[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(o: Origin): Origin = o match {
    case o: Deserialize.Origin => LLVMOrigin(o)
    case other => other
  }
}