package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewritten}
import vct.result.VerificationError.UserError

case object LangSilverToCol {
  case class IncompleteTypeArgs(t: SilverPartialADTFunctionInvocation[_]) extends UserError {
    override def code: String = "incompleteTypeArgs"
    override def text: String =
      t.o.messageInContext("This function invocation does not specify all generic types for the domain.")
  }
}

case class LangSilverToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  import LangSilverToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  def adtInvocation(inv: SilverPartialADTFunctionInvocation[Pre]): Expr[Post] =
    inv.maybeTypeArgs match {
      case None => throw IncompleteTypeArgs(inv)
      case Some(typeArgs) =>
        ADTFunctionInvocation[Post](Some(rw.succ(inv.adt), typeArgs.map(rw.dispatch)), rw.succ(inv.function), inv.args.map(rw.dispatch))(inv.o)
    }

  def nonemptyMap(map: SilverUntypedNonemptyLiteralMap[Pre]): Expr[Post] =
    LiteralMap(rw.dispatch(map.keyType), rw.dispatch(map.valueType), map.values.map { case (k, v) => (rw.dispatch(k), rw.dispatch(v)) })(map.o)
}
