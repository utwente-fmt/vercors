package vct.col.rewrite

import vct.col.ast.{Statement, RangedFor}
import vct.col.util.AstBuildHelpers.const

case object EncodeRangedFor extends RewriterBuilder {
  override def key: String = "encodeRangedFor"
  override def desc: String = "Encodes ranged for as a regular for loop"
}

case class EncodeRangedFor[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
//    case RangedFor(v, from, to, contract, body) => ???
    case stat => rewriteDefault(stat)
  }
}
