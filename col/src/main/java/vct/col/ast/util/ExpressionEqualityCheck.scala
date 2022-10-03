package vct.col.ast.util

import vct.col.ast.util.ExpressionEqualityCheck.isConstantInt
import vct.col.ast.{And, BitAnd, BitNot, BitOr, BitShl, BitShr, BitUShr, BitXor, Div, Eq, Exp, Expr, FloorDiv, Greater, GreaterEq, Implies, IntegerValue, Less, LessEq, Local, Loop, Minus, Mod, Mult, Neq, Not, Or, Plus, Star, UMinus, Wand}
import vct.result.VerificationError.UserError

import scala.collection.mutable

object ExpressionEqualityCheck {
  def apply[G](info: Option[AnnotationVariableInfo[G]] = None): ExpressionEqualityCheck[G] = new ExpressionEqualityCheck[G](info)

  def isConstantInt[G](e: Expr[G]): Option[BigInt] = {
    ExpressionEqualityCheck().isConstantInt(e)
  }

  def equalExpressions[G](lhs: Expr[G], rhs: Expr[G]): Boolean = {
    ExpressionEqualityCheck().equalExpressions(lhs, rhs)
  }
}

case class InconsistentVariableEquality(v: Local[_], x: BigInt, y: BigInt) extends UserError {
  override def code: String = "inconsistentVariableEquality"
  override def text: String = s"Inconsistent variable equality: value of $v is required to be both $x and $y"
}

class ExpressionEqualityCheck[G](info: Option[AnnotationVariableInfo[G]]) {
  var replacerDepth = 0
  var replacerDepthInt = 0
  val max_depth = 100

  def isConstantInt(e: Expr[G]): Option[BigInt] = {
    replacerDepthInt = 0
    isConstantIntRecurse(e)
  }

  def isConstantIntRecurse(e: Expr[G]): Option[BigInt] = e match {
    case e: Local[G] =>
      // Does it have a direct int value?
      info.flatMap(_.variableValues.get(e)) match {
        case Some(x) => Some(x)
        case None =>
          info.flatMap(_.variableEqualities.get(e)) match {
            case None => None
            case Some(equals) =>
              for (eq <- equals) {
                // Make sure we do not loop indefinitely by keep replacing the same expressions somehow
                if (replacerDepthInt > max_depth) return None
                replacerDepthInt += 1
                val res = isConstantIntRecurse(eq)
                if (res.isDefined) return res
              }
              None
          }
      }

    case IntegerValue(value) => Some(value)
    case Exp(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1.pow(i2.toInt)
    case Plus(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 + i2
    case Minus(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 - i2
    case Mult(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 * i2
    case FloorDiv(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 / i2
    case Mod(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 % i2

    case BitAnd(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 & i2
    case BitOr(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 | i2
    case BitXor(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 ^ i2
    case BitShl(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 << i2.toInt
    case BitShr(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1 >> i2.toInt
    case BitUShr(e1, e2) => for {i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)} yield i1.toInt >>> i2.toInt

    case _ => None
  }

  def equalExpressions(lhs: Expr[G], rhs: Expr[G]): Boolean = {
    replacerDepth = 0
    equalExpressionsRecurse(lhs, rhs)
  }

  def isGreaterThanZero(e: Expr[G]): Boolean = e match {
    case v: Local[G] => info.exists(_.variableGreaterThanZero.contains(v))
    case _ => isConstantInt(e).getOrElse(0: BigInt) > 0
  }

  def isNonZero(e: Expr[G]):Boolean = e match {
    case v: Local[G] => info.exists(_.variableNotZero.contains(v))
    case _ => isConstantInt(e).getOrElse(0) != 0
  }

  //
  def equalExpressionsRecurse(lhs: Expr[G], rhs: Expr[G]): Boolean = {
    (isConstantInt(lhs), isConstantInt(rhs)) match {
      case (Some(i1), Some(i2)) => return i1 == i2
      case (None, None) => ()
      //If one is a constant expression, and the other is not, this cannot be the same
      case _ => return false
    }

    (lhs, rhs) match {
      // Unsure if we could check/pattern match on this easier

      // Commutative operators
      case (Plus(lhs1, lhs2), Plus(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))
      case (Mult(lhs1, lhs2), Mult(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))
      case (BitAnd(lhs1, lhs2), BitAnd(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))
      case (BitOr(lhs1, lhs2), BitOr(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))
      case (BitXor(lhs1, lhs2), BitXor(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))
      case (And(lhs1, lhs2), And(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))
      case (Or(lhs1, lhs2), Or(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))
      case (Eq(lhs1, lhs2), Eq(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))
      case (Neq(lhs1, lhs2), Neq(rhs1, rhs2)) =>
        (equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)) ||
          (equalExpressionsRecurse(lhs1, rhs2) && equalExpressionsRecurse(lhs2, rhs1))

      //Non commutative operators
      case (Exp(lhs1, lhs2), Exp(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (Minus(lhs1, lhs2), Minus(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (Div(lhs1, lhs2), Div(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (FloorDiv(lhs1, lhs2), FloorDiv(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (Mod(lhs1, lhs2), Mod(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)

      case (BitShl(lhs1, lhs2), BitShl(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (BitShr(lhs1, lhs2), BitShr(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (BitUShr(lhs1, lhs2), BitUShr(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)

      case (Implies(lhs1, lhs2), Implies(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (Star(lhs1, lhs2), Star(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (Wand(lhs1, lhs2), Wand(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)

      case (Greater(lhs1, lhs2), Greater(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (Less(lhs1, lhs2), Less(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (GreaterEq(lhs1, lhs2), GreaterEq(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)
      case (LessEq(lhs1, lhs2), LessEq(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) && equalExpressionsRecurse(lhs2, rhs2)

      // Unary expressions
      case (UMinus(lhs), UMinus(rhs)) => equalExpressionsRecurse(lhs, rhs)
      case (BitNot(lhs), BitNot(rhs)) => equalExpressionsRecurse(lhs, rhs)
      case (Not(lhs), Not(rhs)) => equalExpressionsRecurse(lhs, rhs)

      // Variables
      case (name1: Local[G], name2: Local[G]) =>
        if (name1 == name2) true
        else if (info.isDefined) {
          // Check if the variables are synonyms
          (info.get.variableSynonyms.get(name1), info.get.variableSynonyms.get(name2)) match {
            case (Some(x), Some(y)) => x == y
            case _ => false
          }
        } else false
      case (name1: Local[G], e2) =>
        replaceVariable(name1, e2)
      case (e1, name2: Local[G]) =>
        replaceVariable(name2, e1)

      // In the general case, we are just interested in syntactic equality
      case (e1, e2) => e1 == e2
    }
  }

  //
  def replaceVariable(name: Local[G], other_e: Expr[G]): Boolean = {
    if (info.isDefined) {
      info.get.variableEqualities.get(name) match {
        case None => false
        case Some(equals) =>
          for (eq <- equals) {
            // Make sure we do not loop indefinitely by keep replacing the same expressions somehow
            if (replacerDepth > max_depth) return false
            replacerDepth += 1
            if (equalExpressionsRecurse(eq, other_e)) return true
          }
          false
      }
    } else {
      false
    }
  }
}

case class AnnotationVariableInfo[G](variableEqualities: Map[Local[G], List[Expr[G]]], variableValues: Map[Local[G], BigInt],
                                     variableSynonyms: Map[Local[G], Int], variableNotZero: Set[Local[G]], variableGreaterThanZero: Set[Local[G]])

/** This class gathers information about variables, such as:
  * `requires x == 0` and stores that x is equal to the value 0.
  * Which we can use in simplify steps
  * This information is returned with ```getInfo(annotations: Iterable[Expr[G]])```
  */
class AnnotationVariableInfoGetter[G]() {

  val variableEqualities: mutable.Map[Local[G], mutable.ListBuffer[Expr[G]]] =
    mutable.Map()
  val variableValues: mutable.Map[Local[G], BigInt] = mutable.Map()
  // We put synonyms in the same group and give them a group number, to identify the same synonym groups
  val variableSynonyms: mutable.Map[Local[G], Int] = mutable.Map()
  val variableNotZero: mutable.Set[Local[G]] = mutable.Set()
  val variableGreaterThanZero: mutable.Set[Local[G]] = mutable.Set()
  var currentSynonymGroup = 0
  var equalCheck: ExpressionEqualityCheck[G] = ExpressionEqualityCheck()

  def extractEqualities(e: Expr[G]): Unit = {
    e match{
      case Eq(e1, e2) =>
        (e1, e2) match{
          case (v1: Local[G], v2: Local[G])  => addSynonym(v1, v2)
          case (v1: Local[G], _)  => addName(v1, e2)
          case (_, v2: Local[G])  => addName(v2, e1)
          case _ =>
        }
      case _ =>
    }
  }

  def extractComparisons(e: Expr[G]): Unit = {
    e match{
      // x != 0
      case Neq(v: Local[G], e2) =>  equalCheck.isConstantInt(e2).foreach{i => if(i == 0) variableNotZero.add(v)}
      // 0 != x
      case Neq(e1, v: Local[G]) => equalCheck.isConstantInt(e1).foreach{i => if(i == 0) variableNotZero.add(v)}
      // x < 0
      case Less(v: Local[G], e2) => equalCheck.isConstantInt(e2).foreach{i => if(i <= 0) variableNotZero.add(v) }
      // x <= -1
      case LessEq(v: Local[G], e2) => equalCheck.isConstantInt(e2).foreach{i => if(i < 0) variableNotZero.add(v) }
      // 0 < x
      case Less(e1, v: Local[G]) => equalCheck.isConstantInt(e1).foreach{i => if(i >= 0) variableNotZero.add(v); variableGreaterThanZero.add(v) }
      // 1 <= x
      case LessEq(e1, v: Local[G]) => equalCheck.isConstantInt(e1).foreach{i => if(i > 0) variableNotZero.add(v); variableGreaterThanZero.add(v) }
      // x > 0
      case Greater(v: Local[G], e2) => equalCheck.isConstantInt(e2).foreach{i => if(i >= 0) variableNotZero.add(v); variableGreaterThanZero.add(v) }
      // x >= 1
      case GreaterEq(v: Local[G], e2) => equalCheck.isConstantInt(e2).foreach{i => if(i > 0) variableNotZero.add(v); variableGreaterThanZero.add(v) }
      // 0 > x
      case Greater(e1, v: Local[G]) => equalCheck.isConstantInt(e1).foreach{i => if(i <= 0) variableNotZero.add(v) }
      // 0 >= x
      case GreaterEq(e1, v: Local[G]) => equalCheck.isConstantInt(e1).foreach{i => if(i < 0) variableNotZero.add(v) }
      case _ =>
    }
  }

  def addSynonym(v1: Local[G], v2: Local[G]): Unit = {
    (variableSynonyms.get(v1), variableSynonyms.get(v2)) match {
      // We make a new group
      case (None, None) =>
        variableSynonyms(v1) = currentSynonymGroup
        variableSynonyms(v2) = currentSynonymGroup
        currentSynonymGroup += 1
      // Add to the found group
      case (Some(id1), None) => variableSynonyms(v2) = id1
      case (None, Some(id2)) => variableSynonyms(v1) = id2
      // Merge the groups, give every synonym group member of id2 value id1
      case (Some(id1), Some(id2)) if id1 != id2 =>
        variableSynonyms.mapValuesInPlace((_, group) => if (group == id2) id1 else group)
      case _ =>
    }
  }

  def addValue(v: Local[G], x: BigInt): Unit =
    variableValues.get(v) match {
      case Some(y) => if (x!=y) throw InconsistentVariableEquality(v, x, y)
      case None =>
        variableValues(v) = x
        if(x>0) variableGreaterThanZero.add(v)
        if(x!=0) variableNotZero.add(v)
    }

  def addName(v: Local[G], expr: Expr[G]): Unit ={
    // Add to constant list
    isConstantInt[G](expr) match {
      case Some(x) => addValue(v, x)
      case None =>
        val list = variableEqualities.getOrElseUpdate(v, mutable.ListBuffer())
        list.addOne(expr)
    }
  }

  def getInfo(annotations: Seq[Expr[G]]): AnnotationVariableInfo[G] = {
    variableEqualities.clear()
    variableValues.clear()
    variableSynonyms.clear()
    currentSynonymGroup = 0
    variableNotZero.clear()
    variableGreaterThanZero.clear()

    for(clause <- annotations){
      extractEqualities(clause)
    }

    val res = AnnotationVariableInfo[G](variableEqualities.view.mapValues(_.toList).toMap, variableValues.toMap,
      variableSynonyms.toMap, Set[Local[G]](), Set[Local[G]]())
    equalCheck = ExpressionEqualityCheck(Some(res))

    for(clause <- annotations){
      extractComparisons(clause)
    }

    distributeInfo()

    AnnotationVariableInfo(variableEqualities.view.mapValues(_.toList).toMap, variableValues.toMap,
      variableSynonyms.toMap, variableNotZero.toSet, variableGreaterThanZero.toSet)
  }

  def distributeInfo(): Unit = {
    // First check if expressions have become integers
    for((name, equals) <- variableEqualities){
      if(!variableValues.contains(name))
        for(equal <- equals){
          equalCheck.isConstantInt(equal) match {
            case Some(x) => addValue(name, x)
            case None =>
          }
        }
    }

    // Group synonym sets
    val synonymSets: mutable.Map[Int, mutable.Set[Local[G]]] = mutable.Map()
    variableSynonyms.foreach{ case (v, groupId) => synonymSets.getOrElse(groupId,mutable.Set()).add(v) }

    def hasValue(vars: mutable.Set[Local[G]]): Option[BigInt] = {
      vars.foreach{v => if(variableValues.contains(v)) return variableValues.get(v) }
      None
    }

    synonymSets.foreach{ case (_, vars) =>
      // Redistribute values over synonyms
      hasValue(vars).foreach{x => vars.foreach{addValue(_, x)}}
      // Redistribute not-zero over synonyms
      if(vars.intersect(variableNotZero).nonEmpty) variableNotZero.addAll(vars)
      // Redistribute greater than zero over synonyms
      if(vars.intersect(variableGreaterThanZero).nonEmpty) variableGreaterThanZero.addAll(vars) }
  }

}