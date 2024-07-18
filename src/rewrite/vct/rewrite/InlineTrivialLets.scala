package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast.{EndpointName, Expr, Let, Local, Variable}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object InlineTrivialLets extends RewriterBuilder {
  override def key: String = "inlineTrivialLets"
  override def desc: String = "Inlines let expressions that only rename a local"
}

case class InlineTrivialLets[Pre <: Generation]() extends Rewriter[Pre] {
  val mappings: ScopedStack[Map[Variable[Pre], Expr[Post]]] = ScopedStack()
  def mapping: Map[Variable[Pre], Expr[Post]] =
    mappings.topOption.getOrElse(Map.empty)

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case Let(
            binder,
            binding @ (_: EndpointName[Pre] | _: Local[Pre]),
            inner,
          ) =>
        mappings.having(mapping.updated(binder, dispatch(binding))) {
          dispatch(inner)
        }
      case Local(Ref(v)) if mapping.contains(v) => mapping(v)
      case _ => expr.rewriteDefault()
    }
}
