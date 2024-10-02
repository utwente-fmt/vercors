package vct.col.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.typerules.CoercingRewriter

case object CIntBoolCoercion extends RewriterBuilder {
  override def key: String = "CIntBool"
  override def desc: String =
    "Introduce an explicit node to convert integers to boolean and vice versa for C."
}

case class CIntBoolCoercion[Pre <: Generation]() extends CoercingRewriter[Pre] {
  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceBoolCInt() =>
        val zero = new CIntegerValue[Post](0)
        val one = new CIntegerValue[Post](1)
        new Select[Post](e, one, zero)
      case CoerceCIntBool() =>
        val zero = new CIntegerValue[Post](0)
        new Neq[Post](e, zero)
      case CoercePointerBool() =>
        val ptr = new Null[Post]()
        new Neq[Post](e, ptr)
      case other => super.applyCoercion(e, other)
    }

}
