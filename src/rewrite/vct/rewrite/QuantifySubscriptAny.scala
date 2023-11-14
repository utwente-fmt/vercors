package vct.col.rewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{DiagnosticOrigin, FramedArrIndex, LabelContext, Origin, PreferredName, TriggerPatternBlame}
import vct.result.VerificationError.UserError

case object QuantifySubscriptAny extends RewriterBuilder {
  override def key: String = "any"
  override def desc: String = "Quantify expressions that are automatically quantified with *."

  val GeneratedQuantifierOrigin: Origin = Origin(
    Seq(
      PreferredName(Seq("i")),
      LabelContext("* index"),
    )
  )

  case class InvalidAnyPosition(any: Any[_]) extends UserError {
    override def code: String = "any"
    override def text: String = any.o.messageInContext(
      "This instance of `*` occurs in a position where VerCors cannot recognize a pattern to quantify.")
  }
}

case class QuantifySubscriptAny[Pre <: Generation]() extends Rewriter[Pre] {
  import QuantifySubscriptAny._

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = GeneratedQuantifierOrigin
    e match {
      case Perm(ArrayLocation(arrIn, any @ Any()), permIn) =>
        val i_var = new Variable[Post](TInt())
        val i = Local[Post](i_var.ref)
        val arr = dispatch(arrIn)
        val perm = dispatch(permIn)

        Starall(
          bindings = Seq(i_var),
          triggers = Seq(
            Seq(ArraySubscript(arr, i)(TriggerPatternBlame))
          ),
          body = Implies(
            const[Post](0) <= i && i < Length(arr)(any.blame),
            Perm(ArrayLocation(arr, i)(FramedArrIndex), perm)
          )
        )(any.blame)
      case Value(ArrayLocation(arrIn, any @ Any())) =>
        val i_var = new Variable[Post](TInt())
        val i = Local[Post](i_var.ref)
        val arr = dispatch(arrIn)
        Starall(
          bindings = Seq(i_var),
          triggers = Seq(
            Seq(ArraySubscript(arr, i)(TriggerPatternBlame))
          ),
          body = Implies(
            const[Post](0) <= i && i < Length(arr)(any.blame),
            Value(ArrayLocation(arr, i)(FramedArrIndex)),
          )
        )(any.blame)
      case node: Any[Pre] => throw InvalidAnyPosition(node)

      case other => rewriteDefault(other)
    }
  }
}
