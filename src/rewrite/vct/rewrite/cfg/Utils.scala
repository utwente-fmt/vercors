package vct.rewrite.cfg

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.{DirectRef, Ref}

object Utils {

  def find_all_subexpressions[G](expr: Expr[G]): Seq[Statement[G]] = expr match {
    case pae @ PreAssignExpression(target, value) => Seq(Assign(target, value)(pae.blame)(pae.o))
    case pae @ PostAssignExpression(target, value) => Seq(Assign(target, value)(pae.blame)(pae.o))
    case With(pre, value) => ???
    case Then(value, post) => ???
    case mi @ MethodInvocation(obj, ref, args, outArgs, typeArgs, givenMap, yields) =>
      Seq(InvokeMethod(obj, ref, args, outArgs, typeArgs, givenMap, yields)(mi.blame)(mi.o))
    case ci @ ConstructorInvocation(ref, args, outArgs, typeArgs, givenMap, yields) =>
      Seq(InvokeConstructor(ref, get_out_variable(ref.decl.cls, ci.o), args, outArgs, typeArgs, givenMap, yields)(ci.blame)(ci.o))
    case pi @ ProcedureInvocation(ref, args, outArgs, typeArgs, givenMap, yields) =>
      Seq(InvokeProcedure(ref, args, outArgs, typeArgs, givenMap, yields)(pi.blame)(pi.o))
    case no @ NewObject(cls) => Seq(Instantiate(cls, get_out_variable(cls, no.o))(no.o))
    // TODO: Consider conditions for control flow graph
    case Select(condition, whenTrue, whenFalse) => Seq(condition, whenTrue, whenFalse).flatMap(e => find_all_subexpressions(e))
    case Implies(left, right) => Seq(left, right).flatMap(e => find_all_subexpressions(e))
    case And(left, right) => Seq(left, right).flatMap(e => find_all_subexpressions(e))
    case Or(left, right) => Seq(left, right).flatMap(e => find_all_subexpressions(e))
    //
    case _ => expr.subnodes.collect{ case ex: Expr[G] => ex }.flatMap(e => find_all_subexpressions(e))
  }

  private def get_out_variable[G](cls: Ref[G, Class[G]],  o: Origin): Local[G] = Local(new DirectRef(new Variable(TClass(cls))(o)))(o)
}
