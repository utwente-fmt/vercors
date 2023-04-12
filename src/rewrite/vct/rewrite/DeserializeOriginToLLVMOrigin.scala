package vct.col.rewrite

import vct.col.ast.Deserialize
import vct.col.origin.{LLVMOrigin, Origin}

case object DeserializeOriginToLLVMOrigin extends RewriterBuilder {
  override def key: String = "DeserializeOriginToLLVMOrigin"

  override def desc: String = "Rewrites all Deserialize.Origin to LLVMOrigin"
}

case class DeserializeOriginToLLVMOrigin[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(o: Origin): Origin = o match {
    case o: Deserialize.Origin => LLVMOrigin(o)
    case other => other
  }
}