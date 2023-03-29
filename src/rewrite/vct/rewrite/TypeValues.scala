package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, Rewritten}

case class TypeValues[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case TypeValue(t) => ???
    case TypeOf(value) => ???
    case InstanceOf(value, typeValue) => ???
    case Cast(value, typeValue) => ???
    case SubType(left, right) => ???
    case SuperType(left, right) => ???
    case other => rewriteDefault(other)
  }
}
