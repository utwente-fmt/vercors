package vct.rewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.rewrite.ResolveExpressionSideEffects
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.InitialGeneration
import vct.col.util.AstBuildHelpers._
import vct.helper.ColHelper

case class ResolveExpressionSideEffectsSpec() extends AnyFlatSpec with Matchers {
  type G = InitialGeneration
  implicit val o: Origin = DiagnosticOrigin
  val rw: ResolveExpressionSideEffects[G] = ResolveExpressionSideEffects()

  val SIDE_EFFECT_1: Statement[G] = Block[G](Nil)
  val SIDE_EFFECT_2: Statement[G] = Block[G](Seq(Block(Nil), Block(Nil)))

  it should "rewrite a pure expression to the same expression" in {
    val simpleExpression = const[G](1) + const(2)
    ColHelper.assertEquals(simpleExpression, rw.dispatch(simpleExpression))
  }

  it should "extract side effects when there is an execution context" in {
    val expr = With(SIDE_EFFECT_1, const(3))
    ColHelper.assertEquals(
      rw.dispatch(Eval(expr)),
      Block(Seq(SIDE_EFFECT_1, Eval[G](const(3))))
    )
  }
}
