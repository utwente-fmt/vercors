package vct.rewrite.cfg

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.{DirectRef, Ref}

import scala.collection.mutable

object Utils {

  def find_all_subexpressions[G](expr: Expr[G]): Seq[Statement[G]] = expr match {
    // Expressions causing side effects
    case pae @ PreAssignExpression(target, value) => Seq(Assign(target, value)(pae.blame)(pae.o))
    case pae @ PostAssignExpression(target, value) => Seq(Assign(target, value)(pae.blame)(pae.o))
    case With(pre, value) => pre +: find_all_subexpressions(value)
    case Then(value, post) => find_all_subexpressions(value) :+ post
    case mi @ MethodInvocation(obj, ref, args, outArgs, typeArgs, givenMap, yields) =>
      Seq(InvokeMethod(obj, ref, args, outArgs, typeArgs, givenMap, yields)(mi.blame)(mi.o))
    case ci @ ConstructorInvocation(ref, args, outArgs, typeArgs, givenMap, yields) =>
      Seq(InvokeConstructor(ref, get_out_variable(ref.decl.cls, ci.o), args, outArgs, typeArgs, givenMap, yields)(ci.blame)(ci.o))
    case pi @ ProcedureInvocation(ref, args, outArgs, typeArgs, givenMap, yields) =>
      Seq(InvokeProcedure(ref, args, outArgs, typeArgs, givenMap, yields)(pi.blame)(pi.o))
    case no @ NewObject(cls) => Seq(Instantiate(cls, get_out_variable(cls, no.o))(no.o))
    // Expressions that affect the edge condition
    case Select(condition, whenTrue, whenFalse) => {
      val statements: Seq[Statement[G]] = find_all_subexpressions(condition)
      val true_stmts: Seq[Statement[G]] = find_all_subexpressions(whenTrue)
      val false_stmts: Seq[Statement[G]] = find_all_subexpressions(whenFalse)
      var branches: Seq[(Expr[G], Statement[G])] = Seq()
      if (true_stmts.nonEmpty) branches = branches.appended((condition, Block(true_stmts)(whenTrue.o)))
      if (false_stmts.nonEmpty) branches = branches.appended((negate(condition), Block(false_stmts)(whenFalse.o)))
      if (branches.nonEmpty) statements ++ Seq(Branch(branches)(expr.o))
      else statements
    }
    case Implies(left, right) => {
      val left_stmts: Seq[Statement[G]] = find_all_subexpressions(left)
      val right_stmts: Seq[Statement[G]] = find_all_subexpressions(right)
      if (right_stmts.nonEmpty) left_stmts :+ Branch(Seq((left, Block(right_stmts)(right.o))))(expr.o)
      else left_stmts
    }
    case And(left, right) => {
      val left_stmts: Seq[Statement[G]] = find_all_subexpressions(left)
      val right_stmts: Seq[Statement[G]] = find_all_subexpressions(right)
      if (right_stmts.nonEmpty) left_stmts :+ Branch(Seq((left, Block(right_stmts)(right.o))))(expr.o)
      else left_stmts
    }
    case Or(left, right) => {
      val left_stmts: Seq[Statement[G]] = find_all_subexpressions(left)
      val right_stmts: Seq[Statement[G]] = find_all_subexpressions(right)
      if (right_stmts.nonEmpty) left_stmts :+ Branch(Seq((negate(left), Block(right_stmts)(right.o))))(expr.o)
      else left_stmts
    }
    // General recursion case
    case _ => expr.subnodes.collect{ case ex: Expr[G] => ex }.flatMap(e => find_all_subexpressions(e))
  }

  private def get_out_variable[G](cls: Ref[G, Class[G]],  o: Origin): Local[G] = Local(new DirectRef[G, Variable[G]](new Variable(TClass(cls))(o)))(o)

  def find_all_cases[G](body: Statement[G], index: GlobalIndex[G]): mutable.Set[(SwitchCase[G], GlobalIndex[G])] = body match {
    case Switch(_, _) => mutable.Set()
    // Recursion on statements that can contain case statements
    case Label(_, stmt) => find_all_cases(stmt, index.enter_scope(body))
    case Block(stmts) => mutable.LinkedHashSet.from(stmts.zipWithIndex.flatMap(t => find_all_cases(t._1, index.enter_scope(body, t._2))))
    case Scope(_, stmt) => find_all_cases(stmt, index.enter_scope(body))
    // Recursion end
    case c: SwitchCase[G] => mutable.Set((c, index))
    case _ => mutable.Set()   // TODO: Assuming that there are no cases in deeper structures (branches, loops etc.)
  }

  def negate[G](expr: Expr[G]): Expr[G] = expr match {
    case Not(inner) => inner
    case _ => Not(expr)(expr.o)
  }

  def and[G](expr1: Option[Expr[G]], expr2: Option[Expr[G]]): Option[Expr[G]] = expr1 match {
    case None => expr2
    case Some(e1) => expr2 match {
      case None => expr1
      case Some(e2) => Some(And(e1, e2)(e1.o))  // TODO: Is the origin important?
    }
  }
}
