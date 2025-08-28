package vct.col.ast.expr.apply

import vct.col.ast.{Applicable, Invocation, Local, Type, Variable}
import vct.col.check.{
  CheckContext,
  CheckError,
  IncorrectArgumentAmount,
  RecursiveFunctionWithoutTerminationMeasure,
}
import vct.col.ref.Ref

trait InvocationImpl[G] extends ApplyImpl[G] {
  this: Invocation[G] =>
  override def t: Type[G] = super.t.particularize(typeEnv)
  def typeEnv: Map[Variable[G], Type[G]]

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    var result = super.check(context)
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
      result =
        result :+
          RecursiveFunctionWithoutTerminationMeasure(this, suggestResult)
    }
    if (ref.decl.args.length != args.length) {
      result =
        result :+
          IncorrectArgumentAmount(this, args.length, ref.decl.args.length)
    }
    result
  }
}
