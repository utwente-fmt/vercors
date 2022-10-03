package vct.col.ast.family.parregion

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, Expr, Local, ParBlock, Starall, Variable}
import vct.col.origin.{Blame, ReceiverNotInjective}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute

trait ParBlockImpl[G] extends Declarator[G] { this: ParBlock[G] =>
  override def declarations: Seq[Declaration[G]] = iters.map(_.variable)

  def quantify(expr: Expr[G], blame: Blame[ReceiverNotInjective]): Expr[G] = {
    val quantVars = iters.map(_.variable).map(v => v -> new Variable[G](v.t)(v.o)).toMap
    val body = Substitute(quantVars.map { case (l, r) => Local[G](l.ref) -> Local[G](r.ref) }.toMap[Expr[G], Expr[G]]).dispatch(expr)
    iters.foldLeft(body)((body, iter) => {
      val v = quantVars(iter.variable)
      Starall(Seq(v), Nil, (iter.from <= v.get && v.get < iter.to) ==> body)(blame)
    })
  }
}