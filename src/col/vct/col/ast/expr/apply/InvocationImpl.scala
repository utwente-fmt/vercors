package vct.col.ast.expr.apply

import vct.col.ast.{Applicable, Invocation, Local, Type, Variable}
import vct.col.check.{
  CheckContext,
  CheckError,
  RecursiveFunctionWithoutTerminationMeasure,
}
import vct.col.ref.Ref

trait InvocationImpl[G] extends ApplyImpl[G] {
  this: Invocation[G] =>
  override def t: Type[G] = super.t.particularize(typeEnv)
  def typeEnv: Map[Variable[G], Type[G]]

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    if (
      context.currentApplicable
        .contains(ref.decl.asInstanceOf[Applicable[G]]) && ref.decl.pure &&
      (context.inPreCondition || context.inPostCondition) &&
      ref.decl.contract.decreases.isEmpty
    ) {
      val suggestResult = args.zip(ref.decl.args).forall {
        case (Local(Ref(a)), p) => a == p
        case _ => false
      }
      Seq(RecursiveFunctionWithoutTerminationMeasure(this, suggestResult))
    } else { Nil }
  }
}
