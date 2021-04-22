package vct.col.ast

import Constant._

object AstBuildHelpers {
  implicit class ExprBuildHelpers(left: Expr) {
    def +(right: Expr)(implicit origin: Origin): Plus = Plus(left, right)
    def -(right: Expr)(implicit origin: Origin): Minus = Minus(left, right)
    def *(right: Expr)(implicit origin: Origin): Mult = Mult(left, right)
    def /(right: Expr)(implicit origin: Origin, blame: DivByZeroBlame): FloorDiv = FloorDiv(left, right)(blame)
    def /:(right: Expr)(implicit origin: Origin, blame: DivByZeroBlame): Div = Div(left, right)(blame)
    def %(right: Expr)(implicit origin: Origin, blame: DivByZeroBlame): Mod = Mod(left, right)(blame)

    def ==(right: Expr)(implicit origin: Origin): Eq = Eq(left, right)
    def !=(right: Expr)(implicit origin: Origin): Neq = Neq(left, right)
    def <(right: Expr)(implicit origin: Origin): Less = Less(left, right)
    def >(right: Expr)(implicit origin: Origin): Greater = Greater(left, right)
    def <=(right: Expr)(implicit origin: Origin): LessEq = LessEq(left, right)
    def >=(right: Expr)(implicit origin: Origin): GreaterEq = GreaterEq(left, right)

    def &&(right: Expr)(implicit origin: Origin): And = And(left, right)
    def ||(right: Expr)(implicit origin: Origin): Or = Or(left, right)
    def &*(right: Expr)(implicit origin: Origin): Star = Star(left, right)

    def ->(field: SilverField)(implicit blame: SilverInsufficientPermissionBlame, origin: Origin): SilverDeref = SilverDeref(left, new DirectRef(field))(blame)

    def @@(index: Expr)(implicit blame: SeqBoundsBlame, origin: Origin): SeqSubscript = SeqSubscript(left, index)(blame)
  }

  implicit class DeclarationBuildHelpers(left: Declaration) {
    def ref: Ref = new DirectRef(left)
  }

  implicit class VarBuildHelpers(left: Variable) {
    def get(implicit origin: Origin): Local = Local(new DirectRef(left))
    def <~(right: Expr)(implicit origin: Origin): SilverLocalAssign = SilverLocalAssign(new DirectRef(left), right)
  }

  implicit class FieldBuildHelpers(left: SilverDeref) {
    def <~(right: Expr)(implicit blame: SilverAssignBlame, origin: Origin): SilverFieldAssign = SilverFieldAssign(left.obj, left.field, right)(blame)
  }

  implicit class StatementBuildHelpers(left: Statement) {

  }
}
