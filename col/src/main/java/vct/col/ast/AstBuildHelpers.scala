package vct.col.ast

import Constant._

import scala.reflect.ClassTag

object AstBuildHelpers {
  implicit class ExprBuildHelpers(left: Expr) {
    def +(right: Expr)(implicit origin: Origin): Plus = Plus(left, right)
    def -(right: Expr)(implicit origin: Origin): Minus = Minus(left, right)
    def *(right: Expr)(implicit origin: Origin): Mult = Mult(left, right)
    def /(right: Expr)(implicit origin: Origin, blame: Blame[DivByZero]): FloorDiv = FloorDiv(left, right)(blame)
    def /:(right: Expr)(implicit origin: Origin, blame: Blame[DivByZero]): Div = Div(left, right)(blame)
    def %(right: Expr)(implicit origin: Origin, blame: Blame[DivByZero]): Mod = Mod(left, right)(blame)

    def ==(right: Expr)(implicit origin: Origin): Eq = Eq(left, right)
    def !=(right: Expr)(implicit origin: Origin): Neq = Neq(left, right)
    def <(right: Expr)(implicit origin: Origin): Less = Less(left, right)
    def >(right: Expr)(implicit origin: Origin): Greater = Greater(left, right)
    def <=(right: Expr)(implicit origin: Origin): LessEq = LessEq(left, right)
    def >=(right: Expr)(implicit origin: Origin): GreaterEq = GreaterEq(left, right)

    def &&(right: Expr)(implicit origin: Origin): And = And(left, right)
    def ||(right: Expr)(implicit origin: Origin): Or = Or(left, right)
    def &*(right: Expr)(implicit origin: Origin): Star = Star(left, right)

    def ==>(right: Expr)(implicit origin: Origin): Implies = Implies(left, right)

    def ~>(field: SilverField)(implicit blame: Blame[SilverInsufficientPermission], origin: Origin): SilverDeref = SilverDeref(left, new DirectRef(field))(blame)

    def @@(index: Expr)(implicit blame: Blame[SeqBoundFailure], origin: Origin): SeqSubscript = SeqSubscript(left, index)(blame)
  }

  implicit class DeclarationBuildHelpers[T <: Declaration](left: T)(implicit tag: ClassTag[T]) {
    def ref: Ref[T] = new DirectRef[T](left)
  }

  implicit class VarBuildHelpers(left: Variable) {
    def get(implicit origin: Origin): Local = Local(new DirectRef(left))
    def <~(right: Expr)(implicit origin: Origin): SilverLocalAssign = SilverLocalAssign(new DirectRef(left), right)
  }

  implicit class FieldBuildHelpers(left: SilverDeref) {
    def <~(right: Expr)(implicit blame: Blame[SilverAssignFailed], origin: Origin): SilverFieldAssign = SilverFieldAssign(left.obj, left.field, right)(blame)
  }

  def procedure(blame: Blame[PostconditionFailed],
                returnType: Type = TVoid(),
                args: Seq[Variable] = Nil, outArgs: Seq[Variable] = Nil,
                body: Option[Statement] = None,
                requires: Expr = new BooleanValue(true)(DiagnosticOrigin), ensures: Expr = new BooleanValue(true)(DiagnosticOrigin),
                contextEverywhere: Expr = new BooleanValue(true)(DiagnosticOrigin),
                signals: Seq[SignalsClause] = Nil,
                givenArgs: Seq[Variable] = Nil, yieldsArgs: Seq[Variable] = Nil,
                inline: Boolean = false, pure: Boolean = false)
               (implicit o: Origin): Procedure =
    new Procedure(returnType, args, outArgs, body,
      ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs),
      inline, pure)(blame)

  def assignField(obj: Expr, field: Ref[Field], value: Expr)(implicit o: Origin): Assign =
    Assign(Deref(obj, field)(DerefAssignTarget), value)

  def fieldPerm(obj: Expr, field: Ref[Field], amount: Expr)(implicit o: Origin): Perm =
    Perm(Deref(obj, field)(DerefPerm), amount)
}
