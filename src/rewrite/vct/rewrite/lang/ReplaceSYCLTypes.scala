package vct.rewrite.lang

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

case object ReplaceSYCLTypes extends RewriterBuilder {
  override def key: String = "replaceSYCLTypes"
  override def desc: String = "Replaces leftover SYCL types with col's 'ref' type."
}

case class ReplaceSYCLTypes[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case _: CPPTLambda[Pre] => TRef()
    case _: SYCLTClass[Pre] => TRef()
    case _ => rewriteDefault(t)
  }
}
