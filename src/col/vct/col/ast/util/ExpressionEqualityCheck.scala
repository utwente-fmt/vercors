package vct.col.ast.util

import vct.col.ast.util.ExpressionEqualityCheck.isConstantInt
import vct.col.ast._
import vct.col.typerules.CoercionUtils
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

import scala.collection.mutable
import scala.reflect.ClassTag

object ExpressionEqualityCheck {
  def apply[G](
      info: Option[AnnotationVariableInfo[G]] = None
  ): ExpressionEqualityCheck[G] = new ExpressionEqualityCheck[G](info)

  def isConstantInt[G](e: Expr[G]): Option[BigInt] = {
    ExpressionEqualityCheck().isConstantInt(e)
  }

  def equalExpressions[G](lhs: Expr[G], rhs: Expr[G]): Boolean = {
    ExpressionEqualityCheck().equalExpressions(lhs, rhs)
  }

  trait Sign
  case class Pos() extends Sign
  case class Neg() extends Sign
}

case class InconsistentVariableEquality(v: Local[_], x: BigInt, y: BigInt)
    extends UserError {
  override def code: String = "inconsistentVariableEquality"
  override def text: String =
    s"Inconsistent variable equality: value of $v is required to be both $x and $y"
}

class ExpressionEqualityCheck[G](info: Option[AnnotationVariableInfo[G]]) {
  import ExpressionEqualityCheck._

  var replacerDepth = 0
  val max_depth = 100

  def isConstantInt(e: Expr[G]): Option[BigInt] = {
    replacerDepth = 0
    isConstantIntRecurse(e)
  }

  def eucl_mod(a: BigInt, b: BigInt): BigInt = {
    val m = a % b
    val absB =
      if (b > 0)
        b
      else
        -b
    if (m <= 0)
      m + absB
    else
      m
  }

  def eucl_div(a: BigInt, b: BigInt): BigInt = {
    val m = a % b
    val d = a / b
    val add =
      if (b > 0)
        -1
      else
        1
    if (m <= 0)
      d + add
    else
      d
  }

  private def isConstantIntRecurse(e: Expr[G]): Option[BigInt] =
    e match {
      case e: Local[G] =>
        // Does it have a direct int value?
        info.flatMap(_.variableValues.get(e)).foreach(x => return Some(x))
        replaceVariable(e).foreach(
          _.foreach(e => isConstantIntRecurse(e).foreach(x => return Some(x)))
        )
        None
      case i: ConstantInt[G] => Some(i.value)
      case Exp(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1.pow(i2.toInt)
      case Plus(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 + i2
      case AmbiguousPlus(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 + i2
      case Minus(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 - i2
      case AmbiguousMinus(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 - i2
      case Mult(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 * i2
      case AmbiguousMult(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 * i2
      case FloorDiv(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield eucl_div(i1, i2)
      case Mod(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield eucl_mod(i1, i2)
      case TruncDiv(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 / i2
      case TruncMod(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 % i2
      case UMinus(e1) => for { i1 <- isConstantIntRecurse(e1) } yield -i1

      case BitAnd(e1, e2, _, _) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 & i2
      case ComputationalAnd(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 & i2
      case AmbiguousComputationalAnd(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 & i2
      case BitOr(e1, e2, _, _) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 | i2
      case ComputationalOr(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 | i2
      case AmbiguousComputationalOr(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 | i2
      case BitXor(e1, e2, _, _) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 ^ i2
      case ComputationalXor(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 ^ i2
      case AmbiguousComputationalXor(e1, e2) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 ^ i2
      case BitShl(e1, e2, _, _) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 << i2.toInt
      case BitShr(e1, e2, _) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1 >> i2.toInt
      case BitUShr(e1, e2, _, _) =>
        for {
          i1 <- isConstantIntRecurse(e1); i2 <- isConstantIntRecurse(e2)
        } yield i1.toInt >>> i2.toInt

      case _ => None
    }

  def equalExpressions(lhs: Expr[G], rhs: Expr[G]): Boolean = {
    replacerDepth = 0
    equalExpressionsRecurse(lhs, rhs)
  }

  def upperBound(e: Expr[G]): Option[BigInt] = {
    replacerDepth = 0; upperBoundRecurse(e)
  }
  def lowerBound(e: Expr[G]): Option[BigInt] = {
    replacerDepth = 0; lowerBoundRecurse(e)
  }
  private def upperBoundRecurse(e: Expr[G]): Option[BigInt] = {
    getBound(e, isLower = false)
  }
  private def lowerBoundRecurse(e: Expr[G]): Option[BigInt] = {
    getBound(e, isLower = true)
  }

  private def getBound(e: Expr[G], isLower: Boolean): Option[BigInt] = {
    isConstantIntRecurse(e).foreach { i => return Some(i) }

    val normalBound =
      if (isLower)
        lowerBound _
      else
        upperBound _
    val reverseBound =
      if (isLower)
        upperBound _
      else
        lowerBound _

    e match {
      case v: Local[G] =>
        info.foreach { i =>
          if (isLower)
            i.lowerBound.get(v).foreach(b => return Some(b))
          else
            i.upperBound.get(v).foreach(b => return Some(b))
        }
        replaceVariable(v) match {
          case Some(es) =>
            es.foreach(e => getBound(e, isLower).foreach(r => return Some(r)))
          case None =>
        }
      case Plus(e1, e2) =>
        return for {
          b1 <- normalBound(e1)
          b2 <- normalBound(e2)
        } yield b1 + b2
      case Minus(e1, e2) =>
        return for {
          b1 <- normalBound(e1)
          b2 <- reverseBound(e2)
        } yield b1 - b2
      case Mult(e1, e2) =>
        isConstantIntRecurse(e1).foreach { i =>
          if (i > 0)
            normalBound(e2).foreach { b2 => return Some(i * b2) }
          if (i < 0)
            reverseBound(e2).foreach { u2 => return Some(i * u2) }
          if (i == 0)
            return Some(0)
        }

        isConstantIntRecurse(e2).foreach { i =>
          if (i > 0)
            normalBound(e1).foreach { b1 => return Some(i * b1) }
          if (i < 0)
            reverseBound(e1).foreach { u1 => return Some(i * u1) }
          if (i == 0)
            return Some(0)
        }

        normalBound(e1).foreach { b1 =>
          normalBound(e2).foreach { b2 =>
            if (b1 > 0 && b2 > 0)
              return Some(b1 * b2)
          }
        }
      case Mod(e1, e2) if isLower => return Some(0)
      case Mod(e1, e2) => isConstantIntRecurse(e2)
      // The other cases are to complicated, so we do not consider them
      case _ =>
    }

    None
  }

  def lessThenEq(lhs: Expr[G], rhs: Expr[G]): Option[Boolean] = {
    replacerDepth = 0
    lessThenEqRecurse(lhs, rhs)
  }

  private def lessThenEqRecurse(lhs: Expr[G], rhs: Expr[G]): Option[Boolean] = {
    // Compare values directly
    (isConstantIntRecurse(lhs), isConstantIntRecurse(rhs)) match {
      case (Some(i1), Some(i2)) => return Some(i1 <= i2)
      case _ =>
    }
    // Compare two variables, where we sometimes store information about
    (lhs, rhs) match {
      case (v1: Local[G], v2: Local[G]) =>
        info.foreach { i =>
          if (
            i.lessThanEqVars.contains(v1) && i.lessThanEqVars(v1).contains(v2)
          )
            return Some(true)
        }
      case _ =>
    }

    // Compare upper and lower bounds of two variables
    (upperBoundRecurse(lhs), lowerBoundRecurse(rhs)) match {
      case (Some(x), Some(y)) if x <= y => return Some(true)
      case _ =>
    }

    None
  }

  def isNonZero(e: Expr[G]): Option[Boolean] = {
    replacerDepth = 0
    isNonZeroRecurse(e)
  }

  private def isNonZeroRecurse(e: Expr[G]): Option[Boolean] = {
    e match {
      case v: Local[G] if info.exists(_.variableNotZero.contains(v)) =>
        return Some(true)
      case v: Local[G] =>
        replaceVariable(v) match {
          case Some(es) =>
            es.foreach(e => isNonZeroRecurse(e).foreach(r => return Some(r)))
          case None =>
        }
      case Mult(l, r) =>
        (isNonZeroRecurse(l), isNonZeroRecurse(r)) match {
          case (Some(l), Some(r)) => return Some(l && r)
          case _ =>
        }
      case _ =>
    }
    isConstantIntRecurse(e).map(i => i != 0) orElse upperBoundRecurse(e)
      .flatMap(i =>
        if (i < 0)
          Some(true)
        else
          None
      ) orElse lowerBoundRecurse(e).flatMap(i =>
      if (i > 0)
        Some(true)
      else
        None
    ) orElse lessThenEq(const(1)(e.o), e) orElse lessThenEq(e, const(-1)(e.o))
  }

  def isSameSign(e1: Expr[G], e2: Expr[G]): Option[Boolean] = {
    replacerDepth = 0;
    isSameSignRecurse(e1, e2)
  }

  private def isSameSignRecurse(e1: Expr[G], e2: Expr[G]): Option[Boolean] = {
    // Try to gets signs
    (getSign(e1), getSign(e2)) match {
      case (Some(s1), Some(s2)) => return Some(s1 == s2)
      case _ =>
    }

    // Determine equal parts of a multiplication
    val (rest_e1, rest_e2) = removeEqExprs(
      unfoldComm[Mult[G]](e1),
      unfoldComm[Mult[G]](e2),
    )

    if (rest_e1.isEmpty && rest_e2.isEmpty)
      return Some(true)

    // Check polarity of rest terms. A negative pol changes the sign.
    val polarity1 =
      rest_e1.map(getSign).foldLeft(true)({
        case (_, None) => return None
        case (p, Some(Pos())) => p
        case (p, Some(Neg())) => !p
      })

    val polarity2 =
      rest_e2.map(getSign).foldLeft(true)({
        case (_, None) => return None
        case (p, Some(Pos())) => p
        case (p, Some(Neg())) => !p
      })

    Some(polarity1 == polarity2)
  }

  def isPos(b: Boolean): Sign =
    if (b)
      Pos()
    else
      Neg()

  def isPos(s: Sign): Boolean =
    s match {
      case Pos() => true
      case Neg() => false
    }

  def getSign(e: Expr[G]): Option[Sign] = {
    isConstantInt(e).map(i => isPos(i >= 0)) orElse lowerBound(e).flatMap(i =>
      if (i >= 0)
        Some(Pos())
      else
        None
    ) orElse upperBound(e).flatMap(i =>
      if (i < 0)
        Some(Neg())
      else
        None
    ) orElse lessThenEq(const(0)(e.o), e).map(isPos)
  }

  def unfoldComm[B <: BinExpr[G]](
      e: Expr[G]
  )(implicit tag: ClassTag[B]): Seq[Expr[G]] = {
    e match {
      case e: B /* checked */ => unfoldComm[B](e.left) ++ unfoldComm[B](e.right)
      case _ => Seq(e)
    }
  }

  def partitionOptionList[A, B](
      xs: Seq[A],
      f: A => Option[B],
  ): (Seq[A], Seq[B]) = {
    var resLeft: Seq[A] = Seq()
    var resRight: Seq[B] = Seq()
    for (x <- xs) {
      f(x) match {
        case Some(b) => resRight ++= Seq(b)
        case None => resLeft ++= Seq(x)
      }
    }
    (resLeft, resRight)
  }

  private def removeEqExprs(
      e1s: Seq[Expr[G]],
      e2s: Seq[Expr[G]],
  ): (Seq[Expr[G]], Seq[Expr[G]]) = {
    var resultingE2: Seq[Expr[G]] = e2s
    var resultingE1: Seq[Expr[G]] = Seq()

    for (x <- e1s) {
      var found = false
      val freezeAvailable = resultingE2
      for (y <- freezeAvailable) {
        if (!found && equalExpressionsRecurse(x, y)) {
          found = true
          resultingE2 = resultingE2.diff(Seq(y))
        }
      }
      if (!found)
        resultingE1 = resultingE1 :+ x
    }
    (resultingE1, resultingE2)
  }

  private def commAssoc[B <: BinExpr[G]](e1: B, e2: B)(
      implicit tag: ClassTag[B]
  ): Boolean = {
    val e1s = unfoldComm[B](e1)
    val e2s = unfoldComm[B](e2)

    val (e1rest, e1Ints) = partitionOptionList(e1s, isConstantInt)
    val (e2rest, e2Ints) = partitionOptionList(e2s, isConstantInt)

    if (e1rest.size != e2rest.size)
      return false

    val res1: Boolean =
      e1 match {
        case _: Plus[G] => e1Ints.sum == e2Ints.sum
        case _: Mult[G] => e1Ints.product == e2Ints.product
        // Should not be reachable
        case _ => ???
      }
    if (!res1)
      return false

    val (e1restrest, e2restrest) = removeEqExprs(e1rest, e2rest)
    return e1restrest.isEmpty && e2restrest.isEmpty
  }

  private def equalExpressionsRecurse(lhs: Expr[G], rhs: Expr[G]): Boolean = {
    (isConstantIntRecurse(lhs), isConstantIntRecurse(rhs)) match {
      case (Some(i1), Some(i2)) => return i1 == i2
      case (None, None) => ()
      // If one is a constant expression, and the other is not, this cannot be the same
      case _ => return false
    }

    def comm(
        lhs1: Expr[G],
        lhs2: Expr[G],
        rhs1: Expr[G],
        rhs2: Expr[G],
    ): Boolean =
      equalExpressionsRecurse(lhs1, rhs1) &&
        equalExpressionsRecurse(lhs2, rhs2) ||
        equalExpressionsRecurse(lhs2, rhs1) &&
        equalExpressionsRecurse(lhs1, rhs2)

    (lhs, rhs) match {
      // Unsure if we could check/pattern match on this easier
      // Commutative operators
      case (lhs @ Plus(_, _), rhs @ Plus(_, _)) => commAssoc[Plus[G]](lhs, rhs)
      case (lhs @ Mult(_, _), rhs @ Mult(_, _)) => commAssoc[Mult[G]](lhs, rhs)
      case (BitAnd(lhs1, lhs2, _, _), BitAnd(rhs1, rhs2, _, _)) =>
        comm(lhs1, lhs2, rhs1, rhs2)
      case (BitOr(lhs1, lhs2, _, _), BitOr(rhs1, rhs2, _, _)) =>
        comm(lhs1, lhs2, rhs1, rhs2)
      case (BitXor(lhs1, lhs2, _, _), BitXor(rhs1, rhs2, _, _)) =>
        comm(lhs1, lhs2, rhs1, rhs2)
      case (And(lhs1, lhs2), And(rhs1, rhs2)) => comm(lhs1, lhs2, rhs1, rhs2)
      case (Or(lhs1, lhs2), Or(rhs1, rhs2)) => comm(lhs1, lhs2, rhs1, rhs2)
      case (Eq(lhs1, lhs2), Eq(rhs1, rhs2)) => comm(lhs1, lhs2, rhs1, rhs2)
      case (Neq(lhs1, lhs2), Neq(rhs1, rhs2)) => comm(lhs1, lhs2, rhs1, rhs2)

      // Non commutative operators
      case (e1: BinExpr[G], e2: BinExpr[G]) =>
        equalExpressionsRecurse(e1.left, e2.left) &&
        equalExpressionsRecurse(e1.right, e2.right)

      case (Star(lhs1, lhs2), Star(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) &&
        equalExpressionsRecurse(lhs2, rhs2)
      case (Wand(lhs1, lhs2), Wand(rhs1, rhs2)) =>
        equalExpressionsRecurse(lhs1, rhs1) &&
        equalExpressionsRecurse(lhs2, rhs2)
      // Unary expressions
      case (e1: UnExpr[G], e2: UnExpr[G]) =>
        equalExpressionsRecurse(e1.arg, e2.arg)

      // Variables
      case (name1: Local[G], name2: Local[G]) =>
        if (name1 == name2)
          true
        else if (info.isDefined) {
          // Check if the variables are synonyms
          (
            info.get.variableSynonyms.get(name1),
            info.get.variableSynonyms.get(name2),
          ) match {
            case (Some(x), Some(y)) => x == y
            case _ => false
          }
        } else
          false
      case (name1: Local[G], e2) =>
        replaceVariable(name1) match {
          case Some(es) => es.exists(e => equalExpressionsRecurse(e, e2))
          case None => false
        }
      case (e1, name2: Local[G]) =>
        replaceVariable(name2) match {
          case Some(es) => es.exists(e => equalExpressionsRecurse(e1, e))
          case None => false
        }
      case (inv: MethodInvocation[G], _) if !inv.ref.decl.pure => false
      case (_, inv: MethodInvocation[G]) if !inv.ref.decl.pure => false

      // In the general case, we are just interested in syntactic equality
      case (e1, e2) => e1 == e2
    }
  }

  def replaceVariable(name: Local[G]): Option[List[Expr[G]]] = {
    if (replacerDepth > max_depth) { return None }
    info.map(_.variableEqualities).flatMap(_.get(name).map(x => {
      replacerDepth += 1; x
    }))
  }
}

case class AnnotationVariableInfo[G](
    variableEqualities: Map[Local[G], List[Expr[G]]],
    variableValues: Map[Local[G], BigInt],
    variableSynonyms: Map[Local[G], Int],
    variableNotZero: Set[Local[G]],
    lessThanEqVars: Map[Local[G], Set[Local[G]]],
    upperBound: Map[Local[G], BigInt],
    lowerBound: Map[Local[G], BigInt],
    usefullConditions: mutable.ArrayBuffer[Expr[G]],
)

/** This class gathers information about variables, such as: `requires x == 0`
  * and stores that x is equal to the value 0. Which we can use in simplify
  * steps This information is returned with ```getInfo(annotations:
  * Iterable[Expr[G]])```
  */
class AnnotationVariableInfoGetter[G]() {

  val variableEqualities: mutable.Map[Local[G], mutable.ListBuffer[Expr[G]]] =
    mutable.Map()
  val variableValues: mutable.Map[Local[G], BigInt] = mutable.Map()
  // We put synonyms in the same group and give them a group number, to identify the same synonym groups
  val variableSynonyms: mutable.Map[Local[G], Int] = mutable.Map()
  val variableNotZero: mutable.Set[Local[G]] = mutable.Set()
  var currentSynonymGroup = 0
  var equalCheck: ExpressionEqualityCheck[G] = ExpressionEqualityCheck()

  // lessThanEqVars(v) = {a,b,c} Captures that variable v is less than or eq to {a,b,c}
  val lessThanEqVars: mutable.Map[Local[G], mutable.Set[Local[G]]] = mutable
    .Map()
  // upperBound(v) = 5 Captures that variable v is less than or equal to 5
  val upperBound: mutable.Map[Local[G], BigInt] = mutable.Map()
  // lowerBound(v) = 5 Captures that variable v is greater than or equal to 5
  val lowerBound: mutable.Map[Local[G], BigInt] = mutable.Map()

  val usefullConditions: mutable.ArrayBuffer[Expr[G]] = mutable.ArrayBuffer()

  def extractEqualities(e: Expr[G]): Unit = {
    e match {
      case Eq(e1, e2) =>
        (e1, e2) match {
          case (v1: Local[G], v2: Local[G]) => addSynonym(v1, v2)
          case (v1: Local[G], _) => addName(v1, e2)
          case (_, v2: Local[G]) => addName(v2, e1)
          case _ =>
        }
      case _ =>
    }
  }

  def addLessEq(v: Local[G], i: BigInt): Unit = {
    val value = upperBound.getOrElse(v, i).min(i)
    upperBound(v) = value
  }

  def addGreaterEq(v: Local[G], i: BigInt): Unit = {
    val value = lowerBound.getOrElse(v, i).max(i)
    lowerBound(v) = value
  }

  def lt(e1: Expr[G], e2: Expr[G], equal: Boolean): Unit = {
    e1 match {
      // x <= i
      case v1: Local[G] if equal =>
        equalCheck.isConstantInt(e2).foreach { i =>
          if (i < 0)
            variableNotZero.add(v1)
          addLessEq(v1, i)
        }
      // x < i
      case v1: Local[G] if !equal =>
        equalCheck.isConstantInt(e2).foreach { i =>
          if (i <= 0)
            variableNotZero.add(v1)
          addLessEq(v1, i - 1)
        }
      case _ =>
    }
    e2 match {
      // i <= x
      case v2: Local[G] if equal =>
        equalCheck.isConstantInt(e1).foreach { i =>
          if (i > 0)
            variableNotZero.add(v2)
          addGreaterEq(v2, i)
        }
      // i < x
      case v2: Local[G] if !equal =>
        equalCheck.isConstantInt(e1).foreach { i =>
          if (i >= 0)
            variableNotZero.add(v2)
          addGreaterEq(v2, i + 1)
        }
      case _ =>
    }

    (e1, e2) match {
      // x < y
      case (v1: Local[G], v2: Local[G]) =>
        lessThanEqVars.getOrElseUpdate(v1, mutable.Set()).addOne(v2)
      case _ =>
    }
  }

  // n == m + 1 then m <= n
  def varEqVarPlusInt(v1: Local[G], v2: Local[G], i: BigInt): Unit = {
    if (i >= 0)
      lessThanEqVars.getOrElseUpdate(v2, mutable.Set()).addOne(v1)
    if (i <= 0)
      lessThanEqVars.getOrElseUpdate(v1, mutable.Set()).addOne(v2)
  }

  // n == m + k,
  // if k>=0 then m <= n
  // if k<=0 then n <= m
  def varEqVarPlusVar(n: Local[G], m: Local[G], k: Local[G]): Unit = {
    if (lowerBound.contains(k) && lowerBound(k) >= 0)
      lessThanEqVars.getOrElseUpdate(m, mutable.Set()).addOne(n)
    if (upperBound.contains(k) && upperBound(k) <= 0)
      lessThanEqVars.getOrElseUpdate(n, mutable.Set()).addOne(m)
    if (lowerBound.contains(m) && lowerBound(m) >= 0)
      lessThanEqVars.getOrElseUpdate(k, mutable.Set()).addOne(n)
    if (upperBound.contains(m) && upperBound(m) <= 0)
      lessThanEqVars.getOrElseUpdate(n, mutable.Set()).addOne(k)
  }

  def isBool[G](e: Expr[G]) = {
    CoercionUtils.getCoercion(e.t, TBool[G]()).isDefined
  }

  def isInt[G](e: Expr[G]) = {
    CoercionUtils.getCoercion(e.t, TInt[G]()).isDefined
  }
  def isSimpleExpr(e: Expr[G]): Boolean = {
    e match {
      case e if (!isInt(e) && !isBool(e)) => false
      case SeqMember(e1, Range(from, to)) =>
        isSimpleExpr(e1) && isSimpleExpr(from) && isSimpleExpr(to)
      case SetMember(e1, RangeSet(from, to)) =>
        isSimpleExpr(e1) && isSimpleExpr(from) && isSimpleExpr(to)
      case e: BinExpr[G] => isSimpleExpr(e.left) && isSimpleExpr(e.right)
      case _: Local[G] => true
      case _: Constant[G] => true
      case _ => false
    }
  }

  def extractComparisons(e: Expr[G]): Unit = {
    e match {
      case Neq(e1, e2) =>
        e1 match {
          // x != 0
          case v1: Local[G] =>
            equalCheck.isConstantInt(e2).foreach { i =>
              if (i == 0)
                variableNotZero.add(v1)
            }
          case _ =>
        }
        e2 match {
          // 0 != x
          case v2: Local[G] =>
            equalCheck.isConstantInt(e1).foreach { i =>
              if (i == 0)
                variableNotZero.add(v2)
            }
          case _ =>
        }
      case Less(e1, e2) => lt(e1, e2, equal = false)
      case LessEq(e1, e2) => lt(e1, e2, equal = true)
      case Greater(e1, e2) => lt(e2, e1, equal = false)
      case GreaterEq(e1, e2) => lt(e2, e1, equal = true)
      case SeqMember(e1, Range(from, to)) =>
        lt(from, e1, equal = true)
        lt(e1, to, equal = false)
      case SetMember(e1, RangeSet(from, to)) =>
        lt(from, e1, equal = true)
        lt(e1, to, equal = false)
      // n == m + 1 then m < n
      case Eq(v1: Local[G], Plus(v2: Local[G], i: ConstantInt[G])) =>
        varEqVarPlusInt(v1, v2, i.value)
      case Eq(v1: Local[G], Plus(i: ConstantInt[G], v2: Local[G])) =>
        varEqVarPlusInt(v1, v2, i.value)
      case Eq(Plus(v2: Local[G], i: ConstantInt[G]), v1: Local[G]) =>
        varEqVarPlusInt(v1, v2, i.value)
      case Eq(Plus(i: ConstantInt[G], v2: Local[G]), v1: Local[G]) =>
        varEqVarPlusInt(v1, v2, i.value)
      case Eq(v1: Local[G], Plus(v2: Local[G], v3: Local[G])) =>
        varEqVarPlusVar(v1, v2, v3)
      case Eq(Plus(v2: Local[G], v3: Local[G]), v1: Local[G]) =>
        varEqVarPlusVar(v1, v2, v3)
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
        variableSynonyms.mapValuesInPlace((_, group) =>
          if (group == id2)
            id1
          else
            group
        )
      case _ =>
    }
  }

  def addValue(v: Local[G], x: BigInt): Unit =
    variableValues.get(v) match {
      case Some(y) =>
        if (x != y)
          throw InconsistentVariableEquality(v, x, y)
      case None =>
        variableValues(v) = x
        if (x != 0)
          variableNotZero.add(v)
        addLessEq(v, x)
        addGreaterEq(v, x)
    }

  def addName(v: Local[G], expr: Expr[G]): Unit = {
    // Add to constant list
    isConstantInt[G](expr) match {
      case Some(x) => addValue(v, x)
      case None =>
        val list = variableEqualities.getOrElseUpdate(v, mutable.ListBuffer())
        list.addOne(expr)
    }
  }

  def addInfo(annotation: Expr[G]): Unit = {
    extractEqualities(annotation)

    if (isSimpleExpr(annotation)) {
      val res = AnnotationVariableInfo[G](
        variableEqualities.view.mapValues(_.toList).toMap,
        variableValues.toMap,
        variableSynonyms.toMap,
        Set[Local[G]](),
        Map[Local[G], Set[Local[G]]](),
        Map[Local[G], BigInt](),
        Map[Local[G], BigInt](),
        usefullConditions,
      )

      equalCheck = ExpressionEqualityCheck(Some(res))
      extractComparisons(annotation)
      usefullConditions.addOne(annotation)
    }
  }

  def finalInfo(): AnnotationVariableInfo[G] = {
    distributeInfo()

    AnnotationVariableInfo(
      variableEqualities.view.mapValues(_.toList).toMap,
      variableValues.toMap,
      variableSynonyms.toMap,
      variableNotZero.toSet,
      lessThanEqVars.view.mapValues(_.toSet).toMap,
      upperBound.toMap,
      lowerBound.toMap,
      usefullConditions,
    )
  }

  def setupInfo(): Unit = {
    variableEqualities.clear()
    variableValues.clear()
    variableSynonyms.clear()
    currentSynonymGroup = 0
    variableNotZero.clear()
    lessThanEqVars.clear()
    upperBound.clear()
    lowerBound.clear()
    usefullConditions.clear()
  }

  def distributeInfo(): Unit = {
    // First check if expressions have become integers
    for ((name, equals) <- variableEqualities) {
      if (!variableValues.contains(name))
        for (equal <- equals) {
          equalCheck.isConstantInt(equal) match {
            case Some(x) => addValue(name, x)
            case None =>
          }
        }
    }

    // Group synonym sets
    val synonymSets: mutable.Map[Int, mutable.Set[Local[G]]] = mutable.Map()
    variableSynonyms.foreach { case (v, groupId) =>
      synonymSets.getOrElse(groupId, mutable.Set()).add(v)
    }

    def hasValue(vars: mutable.Set[Local[G]]): Option[BigInt] = {
      vars.foreach { v =>
        if (variableValues.contains(v))
          return variableValues.get(v)
      }
      None
    }

    def minOption(x: Option[BigInt], y: Option[BigInt]): Option[BigInt] =
      x.map { x => x.min(y.getOrElse(x)) }.orElse(y)
    def maxOption(x: Option[BigInt], y: Option[BigInt]): Option[BigInt] =
      x.map { x => x.max(y.getOrElse(x)) }.orElse(y)

    synonymSets.foreach { case (_, vars) =>
      // Redistribute values over synonyms
      hasValue(vars).foreach { x => vars.foreach { addValue(_, x) } }
      // Redistribute not-zero over synonyms
      if (vars.intersect(variableNotZero).nonEmpty)
        variableNotZero.addAll(vars)

      // Redistribute bounds that were found over synonym set
      var min: Option[BigInt] = None
      var max: Option[BigInt] = None
      vars.foreach { v =>
        min = minOption(upperBound.get(v), min)
        max = maxOption(lowerBound.get(v), max)
      }
      min.foreach { x => vars.foreach { upperBound(_) = x } }
      max.foreach { x => vars.foreach { lowerBound(_) = x } }

      // Collect all vars that are greater than
      val greaterVars: mutable.Set[Local[G]] = mutable.Set()
      vars.foreach { v =>
        lessThanEqVars(v).map { variableSynonyms(_) }.foreach { i =>
          greaterVars.addAll(synonymSets(i))
        }
      }
      // Redistribute all greater vars again
      vars.foreach { lessThanEqVars(_).addAll(greaterVars) }

    }
  }

}
