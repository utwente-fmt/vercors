/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.ast
import viper.silver.ast.pretty.{PrettyExpression, PrettyBinaryExpression, PrettyOperatorExpression, PrettyUnaryExpression, Infix, LeftAssociative, RightAssociative, NonAssociative}
import viper.silver.ast.utility._

/** Expressions. */
sealed trait Exp extends Node with Typed with Positioned with Infoed with PrettyExpression {
  lazy val isPure = Expressions.isPure(this)
  def isHeapDependent(p: Program) = Expressions.isHeapDependent(this, p)

  /**
   * Returns a representation of this expression as it looks when it is used as a proof obligation, i.e. all
   * InhaleExhaleExp are replaced by the inhale part.
   */
  lazy val whenInhaling = Expressions.whenInhaling(this)

  /**
   * Returns a representation of this expression as it looks when it is used as a proof obligation, i.e. all
   * InhaleExhaleExp are replaced by the exhale part.
   */
  lazy val whenExhaling = Expressions.whenExhaling(this)

  /** Returns the subexpressions of this expression */
  lazy val subExps = Expressions.subExps(this)

  // AS: experimenting with removing this code...
  /**
   * Returns a conjunction of all proof obligations in this expression, e.g. rcv != null for all field accesses rcv.f,
   * we have permissions for all field accesses, the preconditions of all used functions are fulfilled etc.
   */
 // lazy val proofObligations = Expressions.proofObligations(this)

}

// --- Simple integer and boolean expressions (binary and unary operations, literals)

// Arithmetic expressions
case class Add(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(AddOp)
case class Sub(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(SubOp)
case class Mul(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(MulOp)
case class Div(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(DivOp)
case class Mod(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(ModOp)

// Integer comparison expressions
case class LtCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(LtOp)
case class LeCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(LeOp)
case class GtCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(GtOp)
case class GeCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(GeOp)

// Equality and non-equality (defined for all types)
case class EqCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends EqualityCmp("==")
case class NeCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends EqualityCmp("!=")

/** Integer literal. */
case class IntLit(i: BigInt)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Literal {
  lazy val typ = Int
}

/** Integer unary minus. */
case class Minus(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainUnExp(NegOp)

// Boolean expressions
case class Or(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(OrOp) {
  Consistency.checkNoPositiveOnly(left)
  Consistency.checkNoPositiveOnly(right)
}
case class And(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(AndOp)

case class Implies(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(ImpliesOp) {
  Consistency.checkNoPositiveOnly(left)
}

case class MagicWand(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo)
    extends DomainBinExp(MagicWandOp) {

  /** Erases all ghost operations such as unfolding from this wand.
    * For example (let A, B and C be free of ghost operations, let P be a predicates,
    * and let W be a wand):
    *
    *     A && unfolding P in B && applying W in C
    *
    * will be transformed into
    *
    *     A && B && C
    *
    * @return The ghost-operations-free version of this wand.
    */
  lazy val withoutGhostOperations: MagicWand = {
    /* We use the post-transformer instead of the pre-transformer in order to
     * perform bottom-up transformation.
     * An alternative would be a pre-transformer and passing a 'recursive'
     * predicate to transform that makes transform recurse if the pre-transformer
     * is defined.
     */
    this.transform()(post = {
      case gop: GhostOperation => gop.body
      case let: Let => let.body
    })
  }

  // maybe rename this sometime
  def subexpressionsToEvaluate(p: Program): Seq[Exp] = {
    this.shallowCollect {
      case old: Old => Some(old)
      case lo: LabelledOld => Some(lo)
      case q: QuantifiedExp if q.isHeapDependent(p) => None
      case e: Exp if !e.isHeapDependent(p) => Some(e)
    }.flatten
  }

  def structurallyMatches(other: MagicWand, p: Program): Boolean = {
    val ignoreExps1 = this.subexpressionsToEvaluate(p)
    val ignoreExps2 = other.subexpressionsToEvaluate(p)

//    println(s"\nignoreExps1 = $ignoreExps1")
//    println(s"ignoreExps2 = $ignoreExps2")

    /* It would suffice to define eq for Exps instead Nodes, but
     * Nodes.children returns a Seq[Nodes]. */
    def eq(e1: Node, e2: Node): Boolean = (e1, e2) match {
      case (`e1`, `e1`) => true
      case _ =>
//        println(s"\ne1 = $e1, e2 = $e2")
        val idx1 = ignoreExps1.indexOf(e1)
//        println(s"idx1 = $idx1")

        if (idx1 >= 0) {
//          println(ignoreExps2(idx1))
//          println(ignoreExps2(idx1) == e2)

          ignoreExps2(idx1) == e2
        } else {
          val b0 = e1.getClass == e2.getClass
//          println(s"  comparing classes: $b0")
          val (subnodes1, otherChildren1) = Nodes.children(e1)
          val (subnodes2, otherChildren2) = Nodes.children(e2)
//          println(s"  subnodes1 = ${subnodes1.toList}\n  otherChildren1 = ${otherChildren1.toList}")
//          println(s"  subnodes2 = ${subnodes2.toList}\n  otherChildren2 = ${otherChildren2.toList}")
          val b1 = subnodes1.zip(subnodes2).forall { case (e1i, e2i) => eq(e1i, e2i)}
//          println(s"  comparing subnodes: $b1")
          val b2 = otherChildren1 == otherChildren2
//          println(s"  comparing other children: $b2")

          (   b0
           && b1
           && b2)
        }
    }

    eq(this.left, other.left) && eq(this.right, other.right)
  }

  override def isValid : Boolean = this match {
    case _ if  left.contains[ForPerm] => false
    case _ if right.contains[ForPerm] => false
    case _ if  left.deepCollect{ case q: Forall if !q.isPure => q }.nonEmpty => false
    case _ if right.deepCollect{ case q: Forall if !q.isPure => q }.nonEmpty => false
    case _ => true
  }
}

/** Boolean negation. */
case class Not(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainUnExp(NotOp) {
  Consistency.checkNoPositiveOnly(exp)
}

/** Boolean literals. */
sealed abstract class BoolLit(val value: Boolean) extends Literal {
  lazy val typ = Bool
}
object BoolLit {
  def unapply(b: BoolLit) = Some(b.value)
  def apply(b: Boolean)(pos: Position = NoPosition, info: Info = NoInfo) = if (b) TrueLit()(pos, info) else FalseLit()(pos, info)
}
case class TrueLit()(val pos: Position = NoPosition, val info: Info = NoInfo) extends BoolLit(true)
case class FalseLit()(val pos: Position = NoPosition, val info: Info = NoInfo) extends BoolLit(false)

case class NullLit()(val pos: Position = NoPosition, val info: Info = NoInfo) extends Literal {
  lazy val typ = Ref
}

// --- Accessibility predicates

/** A common trait for accessibility predicates. */
// Note: adding extra instances of AccessPredicate will require adding cases to viper.silver.ast.utility.multiplyExpByPerm method
sealed trait AccessPredicate extends Exp {
  require(perm isSubtype Perm)
  def loc: LocationAccess
  def perm: Exp
  lazy val typ = Bool
}
object AccessPredicate {
  def unapply(a: AccessPredicate) = Some((a.loc, a.perm))
}

/** An accessibility predicate for a field location. */
case class FieldAccessPredicate(loc: FieldAccess, perm: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AccessPredicate

/** An accessibility predicate for a predicate location. */
case class PredicateAccessPredicate(loc: PredicateAccess, perm: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AccessPredicate

// --- Inhale exhale expressions.

/**
 * This is a special expression that is treated as `in` if it appears as an assumption and as `ex` if
 * it appears as a proof obligation.
 */
case class InhaleExhaleExp(in: Exp, ex: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Exp {
  require(in.typ isSubtype Bool)
  require(ex.typ isSubtype Bool)
  val typ = Bool
}

// --- Permissions

/** A common trait for expressions of type permission. */
sealed trait PermExp extends Exp {
  override lazy val typ = Perm
}

/** A wild card permission. Has an unknown value, but there are no guarantees that it will be the same inside one method. */
case class WildcardPerm()(val pos: Position = NoPosition, val info: Info = NoInfo) extends PermExp

/** The full permission. */
case class FullPerm()(val pos: Position = NoPosition, val info: Info = NoInfo) extends AbstractConcretePerm(1, 1)

/** No permission. */
case class NoPerm()(val pos: Position = NoPosition, val info: Info = NoInfo) extends AbstractConcretePerm(0, 1)

/** An epsilon permission. */
case class EpsilonPerm()(val pos: Position = NoPosition, val info: Info = NoInfo) extends PermExp

/** A concrete fraction as permission amount. */
case class FractionalPerm(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(FracOp) with PermExp
{
  require(left.typ==Int)
  require(right.typ==Int)
}

case class PermDiv(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(PermDivOp) with PermExp
{
  require(left.typ==Perm)
  require(right.typ==Int)
}
/** The permission currently held for a given location. */
case class CurrentPerm(loc: LocationAccess)(val pos: Position = NoPosition, val info: Info = NoInfo) extends PermExp

// Arithmetic expressions
case class PermMinus(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainUnExp(PermNegOp) with PermExp
case class PermAdd(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(PermAddOp) with PermExp
case class PermSub(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(PermSubOp) with PermExp
case class PermMul(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(PermMulOp) with PermExp
case class IntPermMul(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(IntPermMulOp) with PermExp

// Comparison expressions
case class PermLtCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(PermLtOp)
case class PermLeCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(PermLeOp)
case class PermGtCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(PermGtOp)
case class PermGeCmp(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends DomainBinExp(PermGeOp)

// --- Function application (domain and normal)

/** Function application. */
case class FuncApp(funcname: String, args: Seq[Exp])(val pos: Position, val info: Info, override val typ : Type, override val formalArgs: Seq[LocalVarDecl]) extends FuncLikeApp with PossibleTrigger {
  args foreach Consistency.checkNoPositiveOnly
//  args foreach (_.isPure)

  def func : (Program => Function) = (p) => p.findFunction(funcname)
  def getArgs = args
  def withArgs(newArgs: Seq[Exp]) = FuncApp(funcname, newArgs)(pos,info,typ,formalArgs)
  def asManifestation = this
}
// allows a FuncApp to be created directly from a Function node (but only stores its name)
object FuncApp {
  def apply(func: Function, args: Seq[Exp])(pos: Position = NoPosition, info: Info = NoInfo) : FuncApp = FuncApp(func.name, args)(pos,info,func.typ,func.formalArgs)
}

/** User-defined domain function application. */
case class DomainFuncApp(funcname: String, args: Seq[Exp], typVarMap: Map[TypeVar, Type])
                        (val pos: Position, val info: Info, typPassed: => Type, formalArgsPassed: => Seq[LocalVarDecl],val domainName:String)
  extends AbstractDomainFuncApp with PossibleTrigger {
  args foreach Consistency.checkNoPositiveOnly
//  args foreach (_.isPure)
  def typ = typPassed
  def formalArgs = formalArgsPassed
  def func = (p:Program) => p.findDomainFunction(funcname)
  def getArgs = args
  def withArgs(newArgs: Seq[Exp]) = DomainFuncApp(funcname,newArgs,typVarMap)(pos,info,typ,formalArgs,domainName)
  def asManifestation = this
}
object DomainFuncApp {
  def apply(func : DomainFunc, args: Seq[Exp], typVarMap: Map[TypeVar, Type])(pos: Position = NoPosition, info: Info = NoInfo) : DomainFuncApp =
    DomainFuncApp(func.name,args,typVarMap)(pos,info,func.typ.substitute(typVarMap),func.formalArgs map {
    fa =>
      // substitute parameter types
      LocalVarDecl(fa.name, fa.typ.substitute(typVarMap))(fa.pos)
  },func.domainName)
}

// --- Field and predicate accesses

/** A common trait for expressions accessing a location. */
sealed trait LocationAccess extends Exp {
  def loc(p : Program): Location
}

object LocationAccess {
  def unapply(la: LocationAccess) = Some((p:Program) => la.loc(p))
}


/** A field access expression. */
case class FieldAccess(rcv: Exp, field: Field)
                      (val pos: Position = NoPosition, val info: Info = NoInfo)
    extends LocationAccess with Lhs /*with PossibleTrigger*/ {

  def loc(p : Program) = field
  lazy val typ = field.typ

  def getArgs: Seq[Exp] = Seq(rcv)
  def withArgs(args: Seq[Exp]): FieldAccess = copy(rcv = args.head, field)(pos, info)
//  def asManifestation: Exp = this
}

/** A predicate access expression. See also companion object below for an alternative creation signature */
case class PredicateAccess(args: Seq[Exp], predicateName: String)(val pos: Position, val info: Info) extends LocationAccess {
  def loc(p:Program) = p.findPredicate(predicateName)
  lazy val typ = Bool

  /** The body of the predicate with the arguments instantiated correctly. */
  def predicateBody(program : Program) = {
    val predicate = program.findPredicate(predicateName)
    predicate.body map (Expressions.instantiateVariables(_, predicate.formalArgs, args))
  }
}
// allows PredicateAccess to be created from a predicate directly, in which case only the name is kept
object PredicateAccess {
  def apply(args: Seq[Exp], predicate:Predicate)(pos: Position = NoPosition, info: Info = NoInfo) : PredicateAccess = PredicateAccess(args, predicate.name)(pos, info)
}

// --- Conditional expression

/** A conditional expressions. */
case class CondExp(cond: Exp, thn: Exp, els: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo)
    extends Exp with ForbiddenInTrigger {

  require(cond isSubtype Bool)
  Consistency.checkNoPositiveOnly(cond)
  require(thn.typ == els.typ)
  lazy val typ = thn.typ
}

// --- Prover hint expressions

case class Unfolding(acc: PredicateAccessPredicate, body: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Exp {
  Consistency.checkNoPositiveOnly(body)
  lazy val typ = body.typ
}

/* Ghost operations used when packaging magic wands */
sealed trait GhostOperation extends Exp {
  val body: Exp
  lazy val typ = body.typ
}

//sealed trait UnFoldingExp extends GhostOperation {
//  val acc: PredicateAccessPredicate
//}

case class UnfoldingGhostOp(acc: PredicateAccessPredicate, body: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends GhostOperation
case class FoldingGhostOp(acc: PredicateAccessPredicate, body: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends GhostOperation

case class ApplyingGhostOp(exp: Exp, body: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends GhostOperation {
  require(exp isSubtype Wand, s"Expected wand but found ${exp.typ} ($exp)")
}

case class PackagingGhostOp(wand: MagicWand, body: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends GhostOperation {
  require(wand isSubtype Wand, s"Expected wand but found ${wand.typ} ($wand)")
}

// --- Old expressions

sealed trait OldExp extends UnExp {
  lazy val typ = exp.typ
}

case class Old(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends OldExp { Consistency.checkNoPositiveOnly(exp) }
case class ApplyOld(exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends OldExp { Consistency.checkNoPositiveOnly(exp) }

/** Old expression that references a particular state earlier in the program that has been given a name.
  * Evaluates expression in that state. */
case class LabelledOld(exp: Exp, oldLabel: String)(val pos: Position = NoPosition, val info: Info = NoInfo) extends OldExp {
  require(oldLabel != null, "LabelledOld(exp, _): exp cannot be null")
  require(oldLabel != null, "LabelledOld(_, oldLabel): oldLabel cannot be null")
  Consistency.checkNoPositiveOnly(exp)
}

// --- Other expressions

case class Let(variable: LocalVarDecl, exp: Exp, body: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends Exp {
  require(exp.typ isSubtype variable.typ,
          s"Let-bound variable ${variable.name} is of type ${variable.typ}, but bound expression is of type ${exp.typ}")

  val typ = body.typ
}

// --- Quantifications

/** A common trait for quantified expressions. */
sealed trait QuantifiedExp extends Exp {
  require(exp isSubtype Bool)
  def variables: Seq[LocalVarDecl]
  def exp: Exp
  lazy val typ = Bool

  override def isValid : Boolean = this match {
    case _ if contains[MagicWand] => false
    case _ if contains[ForPerm] => false
    case _ if isPure && contains[AccessPredicate] => false
    case _ if !isPure && contains[PredicateAccess] => false
    case _ => true
  }
}

object QuantifiedExp {
  def unapply(q: QuantifiedExp): Option[(Seq[LocalVarDecl], Exp)] = Some(q.variables, q.exp)
}

/** Universal quantification. */
case class Forall(variables: Seq[LocalVarDecl], triggers: Seq[Trigger], exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends QuantifiedExp {
  //require(isValid, s"Invalid quantifier: { $this } .")

  /** Returns an identical forall quantification that has some automatically generated triggers
    * if necessary and possible.
    */
  lazy val autoTrigger: Forall = {
    if (triggers.isEmpty) {
      Expressions.generateTriggerSet(this) match {
        case Some((vars, triggerSets)) =>
          Forall(vars, triggerSets.map(set => Trigger(set.exps)()), exp)(pos, info)
        case None =>
          /* Couldn't generate triggers */
          this
      }
    } else {
      // triggers already present
      this
    }
  }
}

/** Existential quantification. */
case class Exists(variables: Seq[LocalVarDecl], exp: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends QuantifiedExp {
  Consistency.checkNoPositiveOnlyExceptInhaleExhale(exp)
}


/** Quantification over heap chunks with positive permission in any of the listed fields */
case class ForPerm(variable: LocalVarDecl, accessList: Seq[Location], body: Exp)
                  (val pos: Position = NoPosition, val info: Info = NoInfo) extends Exp with QuantifiedExp {
  require(body isSubtype Bool)
  Consistency.checkNoPositiveOnly(body)
  Consistency.recordIfNot(body, Consistency.noPerm(body),
    "forperm expression is not allowed to contain perm expressions")
  Consistency.recordIfNot(body, Consistency.noForPerm(body),
    "forperm expression is not allowed to contain nested forperm expressions")

  def variables: Seq[LocalVarDecl] = Seq(variable)
  def exp: Exp = body

  //TODO: make type of Seq more specific
  override lazy val typ = Bool

  override def isValid : Boolean = this match {
    case _ if body.contains[PermExp] => false
    case ForPerm(_, Seq( Predicate(_, Seq(LocalVarDecl(_, Ref)), _) ), _) => true
    case _ => false
  }
}


/** A trigger for a universally quantified formula. */
case class Trigger(exps: Seq[Exp])(val pos: Position = NoPosition, val info: Info = NoInfo) extends Node with Positioned with Infoed {
  require(exps forall Consistency.validTrigger, s"The trigger { ${exps.mkString(", ")} } is not valid.")
  exps foreach Consistency.checkNoPositiveOnly
}

// --- Variables, this, result

/** A local variable, special or not (used both for declarations and usages). */
sealed trait AbstractLocalVar extends Exp {
  def name: String
  lazy val mutable = true
}
object AbstractLocalVar {
  def unapply(l: AbstractLocalVar) = Some(l.name)
}

/** A normal local variable. */
case class LocalVar(name: String)(val typ: Type, val pos: Position = NoPosition, val info: Info = NoInfo) extends AbstractLocalVar with Lhs {
  require(Consistency.validUserDefinedIdentifier(name))
  require(typ != null)
}

/** A special local variable for the result of a function. */
case class Result()(val typ: Type, val pos: Position = NoPosition, val info: Info = NoInfo) extends AbstractLocalVar {
  lazy val name = "result"
}

// --- Mathematical sequences

/**
 * Marker trait for all sequence-related expressions. Does not imply that the type of the
 * expression is `SeqType`.
 */
sealed trait SeqExp extends Exp with PossibleTrigger

/** The empty sequence of a given element type. */
case class EmptySeq(elemTyp: Type)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp {
  lazy val typ = SeqType(elemTyp)
  def getArgs = Seq()
  def withArgs(newArgs: Seq[Exp]) = this
}

/** An explicit, non-empty sequence. */
case class ExplicitSeq(elems: Seq[Exp])(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp {
  require(elems.nonEmpty)
  require(elems.tail.forall(e => e.typ == elems.head.typ))
  elems foreach Consistency.checkNoPositiveOnly
  lazy val typ = SeqType(elems.head.typ)
  lazy val desugared : SeqExp = {
    elems.toList match {
      case Nil => sys.error("did not expect empty sequence")
      case _ :: Nil => this
      case a :: as => // desugar into singleton sequences and appends
        as.foldLeft[SeqExp](ExplicitSeq(Seq(a))(pos,info)) {
          (bs:SeqExp, b:Exp) => SeqAppend(bs,ExplicitSeq(Seq(b))(pos,info))(pos, info)
        }
    }
  }
  def getArgs = elems
  def withArgs(newArgs: Seq[Exp]) = ExplicitSeq(newArgs)(pos,info)
}

/** A range of integers from 'low' to 'high', not including 'high', but including 'low'. */
case class RangeSeq(low: Exp, high: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp {
  require((low isSubtype Int) && (high isSubtype Int))
  lazy val typ = SeqType(Int)
  def getArgs = Seq(low,high)
  def withArgs(newArgs: Seq[Exp]) = RangeSeq(newArgs.head,newArgs(1))(pos,info)
}

/** Appending two sequences of the same type. */
case class SeqAppend(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp with PrettyBinaryExpression {
  require(left.typ == right.typ)
  lazy val priority = 0
  lazy val fixity = Infix(LeftAssociative)
  lazy val op = "++"
  lazy val typ = left.typ
  def getArgs = Seq(left,right)
  def withArgs(newArgs: Seq[Exp]) = SeqAppend(newArgs.head,newArgs(1))(pos,info)

}

/** Access to an element of a sequence at a given index position (starting at 0). */
case class SeqIndex(s: Exp, idx: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp {
  require(s.typ.isInstanceOf[SeqType])
  require(idx isSubtype Int)
  lazy val typ = s.typ.asInstanceOf[SeqType].elementType
  def getArgs = Seq(s,idx)
  def withArgs(newArgs: Seq[Exp]) = SeqIndex(newArgs.head,newArgs(1))(pos,info)
}

/** Take the first 'n' elements of the sequence 'seq'. */
case class SeqTake(s: Exp, n: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp {
  require(s.typ.isInstanceOf[SeqType])
  require(n isSubtype Int)
  lazy val typ = s.typ
  def getArgs = Seq(s,n)
  def withArgs(newArgs: Seq[Exp]) = SeqTake(newArgs.head,newArgs(1))(pos,info)

}

/** Drop the first 'n' elements of the sequence 'seq'. */
case class SeqDrop(s: Exp, n: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp {
  require(s.typ.isInstanceOf[SeqType])
  require(n isSubtype Int)
  lazy val typ = s.typ
  def getArgs = Seq(s,n)
  def withArgs(newArgs: Seq[Exp]) = SeqDrop(newArgs.head,newArgs(1))(pos,info)

}

/** Is the element 'elem' contained in the sequence 'seq'? */
case class SeqContains(elem: Exp, s: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp with PrettyBinaryExpression {
  require(s.typ.isInstanceOf[SeqType])
  require(elem isSubtype s.typ.asInstanceOf[SeqType].elementType)
  lazy val priority = 0
  lazy val fixity = Infix(LeftAssociative)
  lazy val left: PrettyExpression = elem
  lazy val op = "in"
  lazy val right: PrettyExpression = s
  lazy val typ = Bool
  def getArgs = Seq(elem,s)
  def withArgs(newArgs: Seq[Exp]) = SeqContains(newArgs.head,newArgs(1))(pos,info)
}

/** The same sequence as 'seq', but with the element at index 'idx' replaced with 'elem'. */
case class SeqUpdate(s: Exp, idx: Exp, elem: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp {
  require(s.typ.isInstanceOf[SeqType])
  require(idx isSubtype Int)
  require(elem isSubtype s.typ.asInstanceOf[SeqType].elementType)
  Consistency.checkNoPositiveOnly(elem)
  lazy val desugaredAssumingIndexInRange : SeqExp = {
    SeqAppend(SeqTake(s,idx)(pos,info),SeqAppend(ExplicitSeq(List(elem))(pos,info),SeqDrop(s,Add(idx,IntLit(1)(pos,info))(pos,info))(pos,info))(pos,info))(pos,info)
  }
  lazy val typ = s.typ
  def getArgs = Seq(s,idx,elem)
  def withArgs(newArgs: Seq[Exp]) = SeqUpdate(newArgs.head,newArgs(1),newArgs(2))(pos,info)

}

/** The length of a sequence. */
case class SeqLength(s: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SeqExp {
  require(s.typ.isInstanceOf[SeqType])
  lazy val typ = Int
  def getArgs = Seq(s)
  def withArgs(newArgs: Seq[Exp]) = SeqLength(newArgs.head)(pos,info)

}

// --- Mathematical sets and multisets

/**
 * Marker trait for all set-related expressions. Does not imply that the type of the
 * expression is `SetType`.
 */
sealed trait SetExp extends Exp with PossibleTrigger

/**
 * Marker trait for all set-related expressions. Does not imply that the type of the
 * expression is `MultisetType`.
 */
sealed trait MultisetExp extends Exp with PossibleTrigger

/**
 * Marker traits for all expressions that correspond to operations on sets or multisets.
 * Does not imply that the type of the expression is `SetType` or `MultisetType`.
 */
sealed trait AnySetExp extends SetExp with MultisetExp
sealed trait AnySetUnExp extends AnySetExp with UnExp
sealed trait AnySetBinExp extends AnySetExp with PrettyBinaryExpression with BinExp

/** The empty set of a given element type. */
case class EmptySet(elemTyp: Type)(val pos: Position = NoPosition, val info: Info = NoInfo) extends SetExp {
  lazy val typ = SetType(elemTyp)
  def getArgs = Seq()
  def withArgs(newArgs: Seq[Exp]) = this
}

/** An explicit, non-empty set. */
case class ExplicitSet(elems: Seq[Exp])(val pos: Position = NoPosition, val info: Info = NoInfo) extends SetExp {
  require(elems.nonEmpty)
  require(elems.tail.forall(e => e.typ == elems.head.typ))
  elems foreach Consistency.checkNoPositiveOnly
  lazy val typ = SetType(elems.head.typ)
  def getArgs = elems
  def withArgs(newArgs: Seq[Exp]) = ExplicitSet(newArgs)(pos,info)

}

/** The empty multiset of a given element type. */
case class EmptyMultiset(elemTyp: Type)(val pos: Position = NoPosition, val info: Info = NoInfo) extends MultisetExp {
  lazy val typ = MultisetType(elemTyp)
  def getArgs = Seq()
  def withArgs(newArgs: Seq[Exp]) = this

}

/** An explicit, non-empty multiset. */
case class ExplicitMultiset(elems: Seq[Exp])(val pos: Position = NoPosition, val info: Info = NoInfo) extends MultisetExp {
  require(elems.nonEmpty)
  require(elems.tail.forall(e => e.typ == elems.head.typ))
  elems foreach Consistency.checkNoPositiveOnly
  lazy val typ = MultisetType(elems.head.typ)
  def getArgs = elems
  def withArgs(newArgs: Seq[Exp]) = ExplicitMultiset(newArgs)(pos,info)

}

/** Union of two sets or two multisets. */
case class AnySetUnion(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AnySetBinExp {
  require(left.typ == right.typ)
  require(left.typ.isInstanceOf[SetType] || left.typ.isInstanceOf[MultisetType])
  lazy val priority = 0
  lazy val fixity = Infix(LeftAssociative)
  lazy val op = "union"
  lazy val typ = left.typ
  def getArgs = Seq(left,right)
  def withArgs(newArgs: Seq[Exp]) = AnySetUnion(newArgs.head,newArgs(1))(pos,info)
}

/** Intersection of two sets or two multisets. */
case class AnySetIntersection(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AnySetBinExp {
  require(left.typ == right.typ)
  require(left.typ.isInstanceOf[SetType] || left.typ.isInstanceOf[MultisetType])
  lazy val priority = 0
  lazy val fixity = Infix(LeftAssociative)
  lazy val op = "union"
  lazy val typ = left.typ
  def getArgs = Seq(left,right)
  def withArgs(newArgs: Seq[Exp]) = AnySetIntersection(newArgs.head,newArgs(1))(pos,info)
}

/** Subset relation of two sets or two multisets. */
case class AnySetSubset(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AnySetBinExp {
  require(left.typ == right.typ)
  require(left.typ.isInstanceOf[SetType] || left.typ.isInstanceOf[MultisetType])
  lazy val priority = 0
  lazy val fixity = Infix(NonAssociative)
  lazy val op = "subset"
  lazy val typ = Bool
  def getArgs = Seq(left,right)
  def withArgs(newArgs: Seq[Exp]) = AnySetSubset(newArgs.head,newArgs(1))(pos,info)
}

/** Set difference. */
case class AnySetMinus(left: Exp, right: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AnySetBinExp {
  require(left.typ == right.typ)
  require(left.typ.isInstanceOf[SetType] || left.typ.isInstanceOf[MultisetType])
  lazy val priority = 0
  lazy val fixity = Infix(NonAssociative)
  lazy val op = "setminus"
  lazy val typ = left.typ
  def getArgs = Seq(left,right)
  def withArgs(newArgs: Seq[Exp]) = AnySetMinus(newArgs.head,newArgs(1))(pos,info)
}

/** Is the element 'elem' contained in the sequence 'seq'? */
case class AnySetContains(elem: Exp, s: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AnySetBinExp {
  require((s.typ.isInstanceOf[SetType] && (elem isSubtype s.typ.asInstanceOf[SetType].elementType)) ||
    (s.typ.isInstanceOf[MultisetType] && (elem isSubtype s.typ.asInstanceOf[MultisetType].elementType)))
  lazy val priority = 0
  lazy val fixity = Infix(NonAssociative)
  lazy val left = elem
  lazy val op = "in"
  lazy val right = s
  lazy val typ = if (s.typ.isInstanceOf[SetType]) Bool else Int
  def getArgs = Seq(elem,s)
  def withArgs(newArgs: Seq[Exp]) = AnySetContains(newArgs.head,newArgs(1))(pos,info)
}

/** The length of a sequence. */
case class AnySetCardinality(s: Exp)(val pos: Position = NoPosition, val info: Info = NoInfo) extends AnySetUnExp {
  require(s.typ.isInstanceOf[SetType] || s.typ.isInstanceOf[MultisetType])
  val exp = s
  lazy val typ = Int
  def getArgs = Seq(s)
  def withArgs(newArgs: Seq[Exp]) = AnySetCardinality(newArgs.head)(pos,info)
}

// --- Common functionality

/** Common super trait for all kinds of literals. */
sealed trait Literal extends Exp

/**
 * A common class for concrete permissions.  The name AbstractConcretePerm is used because it is an abstract superclass for concrete permissions.
 */
sealed abstract class AbstractConcretePerm(val numerator: BigInt, val denominator: BigInt) extends PermExp

/**
 * Used to label expression nodes as potentially valid trigger terms for quantifiers.
 * Use ForbiddenInTrigger to declare terms which may not be used in triggers.
 */
sealed trait PossibleTrigger extends Exp {
  def getArgs: Seq[Exp]
  def withArgs(args: Seq[Exp]): PossibleTrigger
}

sealed trait ForbiddenInTrigger extends Exp

/** Common ancestor of Domain Function applications and Function applications. */
sealed trait FuncLikeApp extends Exp with Call {
  def func: Program => FuncLike
  def callee = funcname
  def funcname: String
}
object FuncLikeApp {
  def unapply(fa: FuncLikeApp) = Some((fa.funcname, fa.args))
  def apply(f: FuncLike, args: Seq[Exp], typVars: Map[TypeVar, Type]) = {
    f match {
      case f@Function(_, _, _, _, _, _) => FuncApp(f, args)()
      case f@DomainFunc(_, _, _, _) => DomainFuncApp(f, args, typVars)()
      case _ => sys.error(s"should not occur: $f (${f.getClass})")
    }
  }
}

/** Common superclass for domain functions with arbitrary parameters and return type, binary and unary operations are a special case. */
sealed trait AbstractDomainFuncApp extends FuncLikeApp {
  def func: Program => AbstractDomainFunc
}

/**
 * A common class for equality and inequality comparisons.  Note that equality is defined for
 * all types, and therefore is not a domain function and does not belong to a domain.
 */
sealed abstract class EqualityCmp(val op: String) extends BinExp with PrettyBinaryExpression {
  require(left.typ == right.typ, s"expected the same typ, but got ${left.typ} and ${right.typ}")
  Consistency.checkNoPositiveOnly(left)
  Consistency.checkNoPositiveOnly(right)
  lazy val priority = 13
  lazy val fixity = Infix(NonAssociative)
  lazy val typ = Bool
}

/** Expressions with a unary or binary operator. */
sealed trait DomainOpExp extends AbstractDomainFuncApp with ForbiddenInTrigger { // ForbiddenInTrigger: seems likely that all such operators will not be allowed if/when translated to Z3. But if we need something more fine-grained, we can push this further down the hierarchy.
  def func: Program => Op
}

/** Binary expressions of any kind (whether or not they belong to a domain). */
sealed trait BinExp extends Exp with PrettyBinaryExpression {
  lazy val args = List(left, right)
  def left: Exp
  def right: Exp
}
object BinExp {
  def unapply(binExp: BinExp) = Some((binExp.left, binExp.right))
}

/** Unary expressions of any kind (whether or not they belong to a domain). */
sealed trait UnExp extends Exp {
  lazy val args = List(exp)
  def exp: Exp
}
object UnExp {
  def unapply(unExp: UnExp) = Some(unExp.exp)
}

/** Common superclass for binary expressions that belong to a domain (and thus have a domain operator). */
sealed abstract class DomainBinExp(val funct: BinOp) extends BinExp with DomainOpExp
{
  def func = (_: Program) => funct
  def funcname = funct.name
  def formalArgs = funct.formalArgs
  def op = funct.op
  def fixity = funct.fixity
  def priority = funct.priority
  def typ = funct.typ
}
object DomainBinExp {
  def unapply(e: DomainBinExp) = Some((e.left, e.funct, e.right))
}

/** Common superclass for unary expressions that belong to a domain (and thus have a domain operator). */
sealed abstract class DomainUnExp(val funct: UnOp) extends PrettyUnaryExpression with DomainOpExp with UnExp {
  def func = (_ :Program) => funct
  def funcname = funct.name
  def typ = funct.typ
  def formalArgs = funct.formalArgs
  def op = funct.op
  def fixity = funct.fixity
  def priority = funct.priority
}

/** Expressions which can appear on the left hand side of an assignment */
sealed trait Lhs extends Exp
