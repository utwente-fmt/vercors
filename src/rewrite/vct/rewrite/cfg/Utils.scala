package vct.rewrite.cfg

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.{DirectRef, Ref}

import scala.collection.mutable

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

  def find_all_cases[G](body: Statement[G], index: GlobalIndex[G]): mutable.Set[(SwitchCase[G], GlobalIndex[G])] = body match {
    // Recursion on statements that can contain case statements
    case Label(_, stmt) => find_all_cases(stmt, index.enter_scope(body))
    case Block(stmts) => mutable.Set(stmts.zipWithIndex.flatMap(t => find_all_cases(t._1, index.enter_scope(body, t._2))))
    case Scope(_, stmt) => find_all_cases(stmt, index.enter_scope(body))
    // Recursion end
    case c: SwitchCase[G] => mutable.Set((c, index))
    case _ => mutable.Set()   // TODO: Assuming that there are no cases in deeper structures (branches, loops etc.)
  }
}
