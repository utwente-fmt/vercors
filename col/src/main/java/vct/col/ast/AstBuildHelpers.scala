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

    def ===(right: Expr)(implicit origin: Origin): Eq = Eq(left, right)
    def !=(right: Expr)(implicit origin: Origin): Neq = Neq(left, right)
    def <(right: Expr)(implicit origin: Origin): Less = Less(left, right)
    def >(right: Expr)(implicit origin: Origin): Greater = Greater(left, right)
    def <=(right: Expr)(implicit origin: Origin): LessEq = LessEq(left, right)
    def >=(right: Expr)(implicit origin: Origin): GreaterEq = GreaterEq(left, right)

    def &&(right: Expr)(implicit origin: Origin): And = And(left, right)
    def ||(right: Expr)(implicit origin: Origin): Or = Or(left, right)
    def &*(right: Expr)(implicit origin: Origin): Star = Star(left, right)

    def ==>(right: Expr)(implicit origin: Origin): Implies = Implies(left, right)

    def ~>(field: SilverField)(implicit blame: Blame[InsufficientPermission], origin: Origin): SilverDeref = SilverDeref(left, new DirectRef(field))(blame)

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

  private case object ConstOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def messageInContext(message: String): String = s"[At generated constant]: $message"
  }

  def tt: Constant.BooleanValue = Constant.BooleanValue(true)(ConstOrigin)
  def ff: Constant.BooleanValue = Constant.BooleanValue(false)(ConstOrigin)

  def const(i: Int)(implicit o: Origin): Constant.IntegerValue =
    Constant.IntegerValue(i)

  def contract(requires: Expr = tt, ensures: Expr = tt, contextEverywhere: Expr = tt,
               signals: Seq[SignalsClause] = Nil, givenArgs: Seq[Variable] = Nil, yieldsArgs: Seq[Variable] = Nil)
              (implicit o: Origin): ApplicableContract =
    ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs)

  def procedure(blame: Blame[PostconditionFailed],
                returnType: Type = TVoid(),
                args: Seq[Variable] = Nil, outArgs: Seq[Variable] = Nil, typeArgs: Seq[Variable] = Nil,
                body: Option[Statement] = None,
                requires: Expr = tt, ensures: Expr = tt,
                contextEverywhere: Expr = tt,
                signals: Seq[SignalsClause] = Nil,
                givenArgs: Seq[Variable] = Nil, yieldsArgs: Seq[Variable] = Nil,
                inline: Boolean = false, pure: Boolean = false)
               (implicit o: Origin): Procedure =
    new Procedure(returnType, args, outArgs, typeArgs, body,
      ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs),
      inline, pure)(blame)

  def function(blame: Blame[PostconditionFailed],
               returnType: Type = TVoid(),
               args: Seq[Variable] = Nil, typeArgs: Seq[Variable] = Nil,
               body: Option[Expr] = None,
               requires: Expr = tt, ensures: Expr = tt, contextEverywhere: Expr = tt,
               signals: Seq[SignalsClause] = Nil, givenArgs: Seq[Variable] = Nil, yieldsArgs: Seq[Variable] = Nil,
               inline: Boolean = false)(implicit o: Origin): Function =
    new Function(returnType, args, typeArgs, body,
      ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs),
      inline)(blame)

  def assignField(obj: Expr, field: Ref[Field], value: Expr)(implicit o: Origin): Assign =
    Assign(Deref(obj, field)(DerefAssignTarget), value)

  def fieldPerm(obj: Expr, field: Ref[Field], amount: Expr)(implicit o: Origin): Perm =
    Perm(Deref(obj, field)(DerefPerm), amount)
}
