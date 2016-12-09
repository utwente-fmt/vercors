/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.ast

import viper.silver.ast.utility.{Consistency, Statements, CfgGenerator}

// --- Statements

/** A common trait for statements. */
sealed trait Stmt extends Node with Infoed with Positioned {
  require(Consistency.noResult(this), "Result variables are only allowed in postconditions of functions.")

  /**
   * Returns a list of all actual statements contained in this statement.  That
   * is, all statements except `Seqn`, including statements in the body of loops, etc.
   */
  def children = Statements.children(this)

  /**
   * Returns a control flow graph that corresponds to this statement.
   */
  def toCfg = CfgGenerator.toCFG(this)

  /**
   * Returns a list of all undeclared local variables contained in this statement and
   * throws an exception if the same variable is used with different types.
   */
  def undeclLocalVars = Statements.undeclLocalVars(this)

  /**
   * Computes all local variables that are written to in this statement.
   */
  def writtenVars = Statements.writtenVars(this)
}

/** A statement that creates a new object and assigns it to a local variable. */
case class NewStmt(lhs: LocalVar, fields: Seq[Field])(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(Ref isSubtype lhs)
}

/** An assignment to a field or a local variable */
sealed trait AbstractAssign extends Stmt {
  require(Consistency.isAssignable(rhs, lhs), s"${rhs.typ} ($rhs) is not assignable to ${lhs.typ} ($lhs)")
  Consistency.checkNoPositiveOnly(rhs)
  def lhs: Lhs
  def rhs: Exp
}
object AbstractAssign {
  def apply(lhs: Lhs, rhs: Exp)(pos: Position = NoPosition, info: Info = NoInfo) = lhs match {
    case l: LocalVar => LocalVarAssign(l, rhs)(pos, info)
    case l: FieldAccess => FieldAssign(l, rhs)(pos, info)
  }
  def unapply(a: AbstractAssign) = Some((a.lhs, a.rhs))
}

/** An assignment to a local variable. */
case class LocalVarAssign(lhs: LocalVar, rhs: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AbstractAssign

/** An assignment to a field variable. */
case class FieldAssign(lhs: FieldAccess, rhs: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AbstractAssign

/** A method/function/domain function call. - AS: this comment is misleading - the trait is currently not used for method calls below */
trait Call {
  require(Consistency.areAssignable(args, formalArgs), s"$args vs $formalArgs for callee: $callee")
  def callee: String
  def args: Seq[Exp]
  def formalArgs: Seq[LocalVarDecl] // formal arguments of the call, for type checking
}

/** A method call. */
case class MethodCall private[ast](methodName: String, args: Seq[Exp], targets: Seq[LocalVar])(val pos: Position, val info: Info) extends Stmt {
  // no checks - consistency checks are made below in companion object
}

  object MethodCall {
    def apply(method: Method, args: Seq[Exp], targets: Seq[LocalVar])(pos: Position = NoPosition, info: Info = NoInfo) : MethodCall = {
      require(Consistency.areAssignable(method.formalReturns, targets))
      require(Consistency.noDuplicates(targets))
      args foreach Consistency.checkNoPositiveOnly
      MethodCall(method.name, args, targets)(pos,info)
    }
  }


/** An exhale statement. */
case class Exhale(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(exp isSubtype Bool)
}

/** An inhale statement. */
case class Inhale(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(exp isSubtype Bool)
}

/** An assert statement. */
case class Assert(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(exp isSubtype Bool)
}

/** An fold statement. */
case class Fold(acc: PredicateAccessPredicate)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(acc isSubtype Bool)
}

/** An unfold statement. */
case class Unfold(acc: PredicateAccessPredicate)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(acc isSubtype Bool)
}

/** Package a magic wand. */
case class Package(wand: MagicWand)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(wand isSubtype Wand, s"Expected wand but found ${wand.typ} ($wand)")
}

/** Apply a magic wand. */
case class Apply(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(exp isSubtype Wand, s"Expected wand but found ${exp.typ} ($exp)")
}

/** A sequence of statements. */
case class Seqn(ss: Seq[Stmt])(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt

/** An if control statement. */
case class If(cond: Exp, thn: Stmt, els: Stmt)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  Consistency.checkNoPositiveOnly(cond)
}

/** A while loop. */
case class While(cond: Exp, invs: Seq[Exp], locals: Seq[LocalVarDecl], body: Stmt)
                (val pos: Position = NoPosition, val info: Info = NoInfo)
      extends Stmt {
  Consistency.checkNoPositiveOnly(cond)
  invs foreach Consistency.checkNonPostContract
}

/** A named label. Labels can be used by gotos as jump targets, and by labelled old-expressions
  * to refer to the state as it existed at that label.
  */
case class Label(name: String, invs: Seq[Exp])(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  Consistency.validUserDefinedIdentifier(name)
}

/**
 * A goto statement.  Note that goto's in SIL are limited to forward jumps, and a jump cannot enter
 * a loop but might leave one or several loops.  This ensures that the only back edges in the
 * control flow graph are due to while loops.
 */
case class Goto(target: String)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt

/** A fresh statement assigns a fresh, dedicated symbolic permission values to
  * each of the passed variables.
  */
case class Fresh(vars: Seq[LocalVar])(val pos: Position = NoPosition, val info: Info = NoInfo) extends Stmt {
  require(vars forall (_ isSubtype Perm))
}

/** A constraining-block takes a sequence of permission-typed variables,
  * each of which is marked as constrainable while executing the statements
  * in the body of the block. Potentially constraining statements are, e.g.,
  * exhale-statements.
  */
case class Constraining(vars: Seq[LocalVar], body: Stmt)(val pos: Position = NoPosition, val info: Info = NoInfo)
    extends Stmt {

  require(vars forall (_ isSubtype Perm))
}
