package vct.rewrite.rtos

import vct.col.ast._

import scala.annotation.tailrec

case object Utils {
  def try_expr_to_int(expr: Expr[_]): Option[Int] =
    expr match {
      case IntegerValue(i) => Some(i.intValue)
      case CIntegerValue(i) => Some(i.intValue)
      case UMinus(arg) =>
        try_expr_to_int(arg) match {
          case Some(i) => Some(-i)
          case None => None
        }
      case BitNot(arg) => try_expr_to_int(arg) match {
        case Some(i) => Some(~i)
        case None => None
      }
      case Plus(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 + i2)
      case AmbiguousPlus(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 + i2)
      case Minus(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 - i2)
      case AmbiguousMinus(left, right) =>
        resolve_operator(left, right, (i1, i2) => i1 - i2)
      case AmbiguousMult(left, right) => resolve_operator(left, right, (i1, i2) => i1 * i2)
      case Mult(left, right) => resolve_operator(left, right, (i1, i2) => i1 * i2)
      case AmbiguousDiv(left, right) => resolve_operator(left, right, (i1, i2) => i1 / i2)
      case AmbiguousTruncDiv(left, right) => resolve_operator(left, right, (i1, i2) => i1 / i2)
      case FloorDiv(left, right) => resolve_operator(left, right, (i1, i2) => i1 / i2)
      case AmbiguousMod(left, right) => resolve_operator(left, right, (i1, i2) => i1 % i2)
      case AmbiguousTruncMod(left, right) => resolve_operator(left, right, (i1, i2) => i1 % i2)
      case Mod(left, right) => resolve_operator(left, right, (i1, i2) => i1 % i2)
      case Exp(left, right) => resolve_operator(left, right, (i1, i2) => BigDecimal(i1).pow(i2).intValue)
      case AmbiguousComputationalOr(left, right) => resolve_operator(left, right, (i1, i2) => i1 | i2)
      case ComputationalOr(left, right) => resolve_operator(left, right, (i1, i2) => i1 | i2)
      case AmbiguousComputationalAnd(left, right) => resolve_operator(left, right, (i1, i2) => i1 & i2)
      case ComputationalAnd(left, right) => resolve_operator(left, right, (i1, i2) => i1 & i2)
      case AmbiguousComputationalXor(left, right) => resolve_operator(left, right, (i1, i2) => i1 ^ i2)
      case ComputationalXor(left, right) => resolve_operator(left, right, (i1, i2) => i1 ^ i2)
      case BitAnd(left, right) => resolve_operator(left, right, (i1, i2) => i1 & i2)
      case BitOr(left, right) => resolve_operator(left, right, (i1, i2) => i1 | i2)
      case BitXor(left, right) => resolve_operator(left, right, (i1, i2) => i1 ^ i2)
      case BitShl(left, right) => resolve_operator(left, right, (i1, i2) => i1 << i2)
      case BitShr(left, right) => resolve_operator(left, right, (i1, i2) => i1 >> i2)
      case BitUShr(left, right) => resolve_operator(left, right, (i1, i2) => i1 >>> i2)
      case _ => None
    }

  private def resolve_operator(
      left: Expr[_],
      right: Expr[_],
      op: (Int, Int) => Int,
  ): Option[Int] =
    try_expr_to_int(left) match {
      case Some(i1) =>
        try_expr_to_int(right) match {
          case Some(i2) => Some(op(i1, i2))
          case None => None
        }
      case None => None
    }

  @tailrec
  def get_declarator_name(declarator: CDeclarator[_]): String =
    declarator match {
      case CPointerDeclarator(_, inner) => get_declarator_name(inner)
      case CArrayDeclarator(_, _, inner) => get_declarator_name(inner)
      case CTypeExtensionDeclarator(_, inner) => get_declarator_name(inner)
      case CTypedFunctionDeclarator(_, _, inner) => get_declarator_name(inner)
      case CAnonymousFunctionDeclarator(_, inner) => get_declarator_name(inner)
      case CName(name) => name
    }

  def get_applicable_name(applicable: Expr[_]): String =
    applicable match {
      case CLocal(name) => name
      case _ =>
        throw new IllegalArgumentException(
          "Applicable " + applicable.toInlineString +
            " has unexpected node type!"
        )
    }
}
