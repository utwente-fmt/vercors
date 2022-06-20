package vct.col.ast.util

import hre.lang.System.Warning
import vct.col.ast.util.ExpressionEqualityCheck.is_constant_int
import vct.col.ast.{And, BitAnd, BitNot, BitOr, BitShl, BitShr, BitUShr, BitXor, Div, Eq, Exp, Expr, FloorDiv, Greater, GreaterEq, Implies, IntegerValue, Less, LessEq, Local, Minus, Mod, Mult, Neq, Not, Or, Plus, Star, UMinus, Wand}

import scala.collection.mutable

object ExpressionEqualityCheck {
  def apply[G](info: Option[AnnotationVariableInfo[G]] = None): ExpressionEqualityCheck[G] = new ExpressionEqualityCheck[G](info)

  def is_constant_int[G](e: Expr[G]): Option[BigInt] = {
    ExpressionEqualityCheck().is_constant_int(e)
  }

  def equal_expressions[G](lhs: Expr[G], rhs: Expr[G]): Boolean = {
    ExpressionEqualityCheck().equal_expressions(lhs, rhs)
  }
}

class ExpressionEqualityCheck[G](info: Option[AnnotationVariableInfo[G]]) {
  var replacer_depth = 0
  var replacer_depth_int = 0
  val max_depth = 100

  def is_constant_int(e: Expr[G]): Option[BigInt] = {
    replacer_depth_int = 0
    is_constant_int_(e)
  }

  def is_constant_int_(e: Expr[G]): Option[BigInt] = e match {
    case e: Local[G] =>
      // Does it have a direct int value?
      info.flatMap(_.variable_values.get(e)) match {
        case Some(x) => Some(x)
        case None =>
          info.flatMap(_.variable_equalities.get(e)) match {
            case None => None
            case Some(equals) =>
              for (eq <- equals) {
                // Make sure we do not loop indefinitely by keep replacing the same expressions somehow
                if (replacer_depth_int > max_depth) return None
                replacer_depth_int += 1
                val res = is_constant_int_(eq)
                if (res.isDefined) return res
              }
              None
          }
      }

    case IntegerValue(value) => Some(value)
    case Exp(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1.pow(i2.toInt)
    case Plus(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 + i2
    case Minus(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 - i2
    case Mult(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 * i2
    case FloorDiv(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 / i2
    case Mod(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 % i2

    case BitAnd(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 & i2
    case BitOr(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 | i2
    case BitXor(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 ^ i2
    case BitShl(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 << i2.toInt
    case BitShr(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1 >> i2.toInt
    case BitUShr(e1, e2) => for {i1 <- is_constant_int_(e1); i2 <- is_constant_int_(e2)} yield i1.toInt >>> i2.toInt

    case _ => None
  }

  def equal_expressions(lhs: Expr[G], rhs: Expr[G]): Boolean = {
    replacer_depth = 0
    equal_expressions_(lhs, rhs)
  }

  //
  def equal_expressions_(lhs: Expr[G], rhs: Expr[G]): Boolean = {
    (is_constant_int(lhs), is_constant_int(rhs)) match {
      case (Some(i1), Some(i2)) => return i1 == i2
      case (None, None) => ()
      //If one is a constant expression, and the other is not, this cannot be the same
      case _ => return false
    }

    (lhs, rhs) match {
      // Unsure if we could check/pattern match on this easier

      // Commutative operators
      case (Plus(lhs1, lhs2), Plus(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))
      case (Mult(lhs1, lhs2), Mult(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))
      case (BitAnd(lhs1, lhs2), BitAnd(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))
      case (BitOr(lhs1, lhs2), BitOr(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))
      case (BitXor(lhs1, lhs2), BitXor(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))
      case (And(lhs1, lhs2), And(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))
      case (Or(lhs1, lhs2), Or(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))
      case (Eq(lhs1, lhs2), Eq(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))
      case (Neq(lhs1, lhs2), Neq(rhs1, rhs2)) =>
        (equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)) ||
          (equal_expressions_(lhs1, rhs2) && equal_expressions_(lhs2, rhs1))

      //Non commutative operators
      case (Exp(lhs1, lhs2), Exp(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (Minus(lhs1, lhs2), Minus(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (Div(lhs1, lhs2), Div(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (FloorDiv(lhs1, lhs2), FloorDiv(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (Mod(lhs1, lhs2), Mod(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)

      case (BitShl(lhs1, lhs2), BitShl(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (BitShr(lhs1, lhs2), BitShr(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (BitUShr(lhs1, lhs2), BitUShr(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)

      case (Implies(lhs1, lhs2), Implies(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (Star(lhs1, lhs2), Star(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (Wand(lhs1, lhs2), Wand(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)

      case (Greater(lhs1, lhs2), Greater(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (Less(lhs1, lhs2), Less(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (GreaterEq(lhs1, lhs2), GreaterEq(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)
      case (LessEq(lhs1, lhs2), LessEq(rhs1, rhs2)) =>
        equal_expressions_(lhs1, rhs1) && equal_expressions_(lhs2, rhs2)

      // Unary expressions
      case (UMinus(lhs), UMinus(rhs)) => equal_expressions_(lhs, rhs)
      case (BitNot(lhs), BitNot(rhs)) => equal_expressions_(lhs, rhs)
      case (Not(lhs), Not(rhs)) => equal_expressions_(lhs, rhs)

      // Variables
      case (name1: Local[G], name2: Local[G]) =>
        if (name1 == name2) true
        else if (info.isDefined) {
          // Check if the variables are synonyms
          (info.get.variable_synonyms.get(name1), info.get.variable_synonyms.get(name2)) match {
            case (Some(x), Some(y)) => x == y
            case _ => false
          }
        } else false
      case (name1: Local[G], e2) =>
        replace_variable(name1, e2)
      case (e1, name2: Local[G]) =>
        replace_variable(name2, e1)

      // In the general case, we are just interested in syntactic equality
      case (e1, e2) => e1 == e2
    }
  }

  //
  def replace_variable(name: Local[G], other_e: Expr[G]): Boolean = {
    if (info.isDefined) {
      info.get.variable_equalities.get(name) match {
        case None => false
        case Some(equals) =>
          for (eq <- equals) {
            // Make sure we do not loop indefinitely by keep replacing the same expressions somehow
            if (replacer_depth > max_depth) return false
            replacer_depth += 1
            if (equal_expressions_(eq, other_e)) return true
          }
          false
      }
    } else {
      false
    }
  }
}

case class AnnotationVariableInfo[G](variable_equalities: Map[Local[G], List[Expr[G]]], variable_values: Map[Local[G], BigInt],
                                 variable_synonyms: Map[Local[G], Int])

/** This class gathers information about variables, such as:
  * `requires x == 0` and stores that x is equal to the value 0.
  * Which we can use in simplify steps
  * This information is returned with ```get_info(annotations: Iterable[Expr[G]])```
  */
class AnnotationVariableInfoGetter[G]() {

  val variable_equalities: mutable.Map[Local[G], mutable.ListBuffer[Expr[G]]] =
    mutable.Map()
  val variable_values: mutable.Map[Local[G], BigInt] = mutable.Map()
  // We put synonyms in the same group and give them a group number, to identify the same synonym groups
  val variable_synonyms: mutable.Map[Local[G], Int] = mutable.Map()
  var current_synonym_group = 0

  def extract_equalities(e: Expr[G]): Unit = {
    e match{
      case Eq(e1, e2) =>
        (e1, e2) match{
          case (v1: Local[G], v2: Local[G])  => add_synonym(v1, v2)
          case (v1: Local[G], _)  => add_name(v1, e2)
          case (_, v2: Local[G])  => add_name(v2, e1)
          case _ =>
        }
      case _ =>
    }
  }

  def add_synonym(v1: Local[G], v2: Local[G]): Unit = {
    (variable_synonyms.get(v1), variable_synonyms.get(v2)) match {
      // We make a new group
      case (None, None) =>
        variable_synonyms(v1) = current_synonym_group
        variable_synonyms(v2) = current_synonym_group
        current_synonym_group += 1
      // Add to the found group
      case (Some(id1), None) => variable_synonyms(v2) = id1
      case (None, Some(id2)) => variable_synonyms(v1) = id2
      // Merge the groups, give every synonym group member of id2 value id1
      case (Some(id1), Some(id2)) =>
        variable_synonyms.mapValuesInPlace((_, group) => if (group == id2) id1 else group)
    }
  }

  def add_name(v: Local[G], expr: Expr[G]): Unit ={
    // Add to constant list
    is_constant_int[G](expr) match {
      case Some(x) => variable_values.get(v) match {
        case Some(x_) => if (x!=x_) Warning("Value of %s is required to be both %d and %d", v, x, x_);
        case None => variable_values(v) = x
      }
      case None =>
        val list = variable_equalities.getOrElseUpdate(v, mutable.ListBuffer())
        list.addOne(expr)
    }
  }

  def get_info(annotations: Iterable[Expr[G]]): AnnotationVariableInfo[G] = {
    variable_equalities.clear()
    variable_values.clear()

    for(clause <- annotations){
      extract_equalities(clause)
    }

    distribute_info()
    AnnotationVariableInfo(variable_equalities.view.mapValues(_.toList).toMap, variable_values.toMap,
      variable_synonyms.toMap)
  }

  def distribute_info(): Unit = {
    // First distribute value knowledge over the rest of the map
    val begin_size = variable_values.size

    for((name, equals) <- variable_equalities){
      if(!variable_values.contains(name))
        for(equal <- equals){
          equal match {
            case n : Local[G] =>
              variable_values.get(n).foreach(variable_values(name) = _)
            case _ =>
          }
        }
    }

    // If sizes are not the same, we know more, so distribute again!
    if(variable_values.size != begin_size) distribute_info()
  }

}