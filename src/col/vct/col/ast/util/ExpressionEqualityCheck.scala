package vct.col.ast.util

import com.typesafe.scalalogging.LazyLogging
import org.sosy_lab.common.ShutdownNotifier
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.LogManager
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula
import org.sosy_lab.java_smt.api.SolverContext.ProverOptions
import org.sosy_lab.java_smt.api._
import vct.col.ast.util.ExpressionEqualityCheck.{
  isConstantInt,
  isValidSymbolicTerm,
}
import vct.col.ast._
import vct.col.origin._
import vct.col.typerules.CoercionUtils
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Using

object ExpressionEqualityCheck extends LazyLogging {
  def apply[G](
      info: Option[AnnotationVariableInfo[G]] = None
  ): ExpressionEqualityCheck[G] = new ExpressionEqualityCheck[G](info)

  def isConstantInt[G](e: Expr[G]): Option[BigInt] = {
    ExpressionEqualityCheck().isConstantInt(e)
  }

  def equalExpressions[G](lhs: Expr[G], rhs: Expr[G]): Boolean = {
    ExpressionEqualityCheck().equalExpressions(lhs, rhs)
  }

  def isValidSymbolicTerm(term: SymbolicTerm[_]): Boolean =
    term match {
      case Local(_) => true
      case OptEmpty(inner) => stricterIsConstant(inner)
      case ADTFunctionInvocation(_, _, _) => true
      case ProverFunctionInvocation(_, _) => true
      case invocation: AnyFunctionInvocation[_] =>
        !invocation.ref.decl.contract.exists { case _: ResourceTerm[_] =>
          true
        } && invocation.givenMap.forall(it => stricterIsConstant(it._2)) &&
        invocation.args.forall(stricterIsConstant)
    }

  def stricterIsConstant(e: Expr[_]): Boolean = {
    def rec(e: Expr[_]) = stricterIsConstant(e)
    e match {
      case inv: AnyFunctionInvocation[_] =>
        // We need the isValidSymbolicTerm here since the symbolic terms appearing inside of this expression have not previously been checked, unlike the calls to the isConstant function below
        inv.args.forall(rec) && inv.givenMap.map(_._2).forall(rec) &&
        inv.yields.map(_._1).forall(rec) && isValidSymbolicTerm(inv)
      case t: SymbolicTerm[_] if isValidSymbolicTerm(t) => true
      case _: Constant[_] => true
      case e: UnExpr[_] => rec(e.arg)
      case e: BinExpr[_] => rec(e.left) && rec(e.right)
      case _ => false
    }
  }

  trait Sign
  case class Pos() extends Sign
  case class Neg() extends Sign

  case class AskSMTSolver[G](constraints: Iterable[Expr[G]], test: Expr[G]) {
    private implicit val o: Origin = Origin(
      Seq(PreferredName(Seq("unknown")), LabelContext("simplification"))
    )

    def isBool(e: Expr[_]): Boolean = {
      CoercionUtils.getCoercion(e.t, TBool()).isDefined
    }

    def isInt(e: Expr[_]): Boolean = {
      CoercionUtils.getCoercion(e.t, TInt()).isDefined
    }

    def check(): Boolean = {
      val options = Configuration.builder()
        .setOption("solver.nonLinearArithmetic", "APPROXIMATE_FALLBACK")
      val config = options.build()
      val logManager = LogManager.createNullLogManager
      val shutDown = ShutdownNotifier.createDummy
      var id = 0

      Using(SolverContextFactory.createSolverContext(
        config,
        logManager,
        shutDown,
        Solvers.SMTINTERPOL,
      )) { ctx =>
        Using(ctx.newProverEnvironment(ProverOptions.GENERATE_MODELS)) {
          prover =>
            val fmgr = ctx.getFormulaManager
            val varIntMap: mutable.Map[SymbolicTerm[G], IntegerFormula] =
              mutable.Map()
            val varBoolMap: mutable.Map[SymbolicTerm[G], BooleanFormula] =
              mutable.Map()

            val takenNames: mutable.Set[String] = mutable.Set()
            val bmgr = fmgr.getBooleanFormulaManager
            val imgr = fmgr.getIntegerFormulaManager

            def tdiv(a: Expr[G], b: Expr[G]): Expr[G] =
              Select(
                a >= const(0) || a % b === const(0),
                a / b,
                a / b + Select(b > const(0), const(1), const(-1)),
              )

            def tmod(a: Expr[G], b: Expr[G]): Expr[G] =
              Select(
                a >= const(0) || a % b === const(0),
                a % b,
                a % b - Select(b > const(0), b, -b),
              )

            def addConstraint(e: Expr[G]): Boolean = {
              addBool(e) match {
                case Some(b1) => prover.addConstraint(b1); true
                case None => false
              }
            }

            def getName(t: SymbolicTerm[G]): String = {
              val name = t.o.getPreferredNameOrElse().camel
              var pname = name
              var i = 0
              while (takenNames.contains(pname)) {
                pname = s"$name$i"
                i += 1
              }
              if (!fmgr.isValidName(pname)) {
                pname = s"b$id"
                while (takenNames.contains(pname)) {
                  id += 1
                  pname = s"b$id"
                }
                id += 1
              }
              takenNames.add(pname)
              pname
            }

            def addBool(e: Expr[G]): Option[BooleanFormula] = {
              e match {
                case Select(c, t, f) =>
                  for {
                    c1 <- addBool(c); t1 <- addBool(t); f1 <- addBool(f)
                  } yield bmgr.ifThenElse(c1, t1, f1)
                case SeqMember(e1, Range(from, to)) =>
                  for {
                    i1 <- addInt(e1); fromi <- addInt(from); toi <- addInt(to)
                  } yield bmgr
                    .and(imgr.lessOrEquals(fromi, i1), imgr.lessThan(i1, toi))
                case SetMember(e1, RangeSet(from, to)) =>
                  for {
                    i1 <- addInt(e1); fromi <- addInt(from); toi <- addInt(to)
                  } yield bmgr
                    .and(imgr.lessOrEquals(fromi, i1), imgr.lessThan(i1, toi))
                case Or(e1, e2) =>
                  for {
                    b1 <- addBool(e1); b2 <- addBool(e2)
                  } yield bmgr.or(b1, b2)
                case And(e1, e2) =>
                  for {
                    b1 <- addBool(e1); b2 <- addBool(e2)
                  } yield bmgr.and(b1, b2)
                case Implies(e1, e2) =>
                  for {
                    b1 <- addBool(e1); b2 <- addBool(e2)
                  } yield bmgr.implication(b1, b2)
                case Not(e1) => for { b1 <- addBool(e1) } yield bmgr.not(b1)
                case Eq(e1, e2) if isBool(e1) && isBool(e2) =>
                  for {
                    b1 <- addBool(e1); b2 <- addBool(e2)
                  } yield bmgr.equivalence(b1, b2)
                case Eq(e1, e2) if isInt(e1) && isInt(e2) =>
                  for {
                    b1 <- addInt(e1); b2 <- addInt(e2)
                  } yield imgr.equal(b1, b2)
                case Neq(e1, e2) if isInt(e1) && isInt(e2) =>
                  for {
                    b1 <- addInt(e1); b2 <- addInt(e2)
                  } yield bmgr.not(imgr.equal(b1, b2))
                case Less(e1, e2) if isInt(e1) && isInt(e2) =>
                  for {
                    b1 <- addInt(e1); b2 <- addInt(e2)
                  } yield imgr.lessThan(b1, b2)
                case LessEq(e1, e2) if isInt(e1) && isInt(e2) =>
                  for {
                    b1 <- addInt(e1); b2 <- addInt(e2)
                  } yield imgr.lessOrEquals(b1, b2)
                case Greater(e1, e2) if isInt(e1) && isInt(e2) =>
                  for {
                    b1 <- addInt(e1); b2 <- addInt(e2)
                  } yield imgr.greaterThan(b1, b2)
                case GreaterEq(e1, e2) if isInt(e1) && isInt(e2) =>
                  for {
                    b1 <- addInt(e1); b2 <- addInt(e2)
                  } yield imgr.greaterOrEquals(b1, b2)
                case BooleanValue(b) => Some(bmgr.makeBoolean(b))
                case t: SymbolicTerm[G]
                    if isBool(e) && isValidSymbolicTerm(t) =>
                  if (varBoolMap.contains(t))
                    Some(varBoolMap(t))
                  else {
                    val name = getName(t)
                    val x = bmgr.makeVariable(name)
                    varBoolMap(t) = x
                    Some(x)
                  }
                case _ => None
              }
            }

            def addInt(e: Expr[G]): Option[IntegerFormula] = {
              e match {
                case Select(c, t, f) =>
                  for {
                    c1 <- addBool(c); t1 <- addInt(t); f1 <- addInt(f)
                  } yield bmgr.ifThenElse(c1, t1, f1)
                case Plus(e1, e2) =>
                  for {
                    i1 <- addInt(e1); i2 <- addInt(e2)
                  } yield imgr.add(i1, i2)
                case Minus(e1, e2) =>
                  for {
                    i1 <- addInt(e1); i2 <- addInt(e2)
                  } yield imgr.subtract(i1, i2)
                case Mult(e1, e2) =>
                  for {
                    i1 <- addInt(e1); i2 <- addInt(e2)
                  } yield imgr.multiply(i1, i2)
                case FloorDiv(e1, e2) =>
                  for {
                    i1 <- addInt(e1); i2 <- addInt(e2)
                  } yield imgr.divide(i1, i2)
                // Ugly, but the SMT library does not allow us to define functions..
                case TruncDiv(e1, e2) => addInt(tdiv(e1, e2))
                case TruncMod(e1, e2) => addInt(tmod(e1, e2))
                case Mod(e1, e2) =>
                  for {
                    i1 <- addInt(e1); i2 <- addInt(e2)
                  } yield imgr.modulo(i1, i2)
                case UMinus(e1) =>
                  for { i1 <- addInt(e1) } yield imgr.negate(i1)
                case IntegerValue(i) => Some(imgr.makeNumber(i.toInt))
                case t: SymbolicTerm[G] if isInt(e) && isValidSymbolicTerm(t) =>
                  if (varIntMap.contains(t))
                    Some(varIntMap(t))
                  else {
                    val name = getName(t)
                    val x = imgr.makeVariable(name)
                    varIntMap(t) = x
                    Some(x)
                  }
                case _ => None
              }
            }

            for (c <- constraints) {
              if (!addConstraint(c))
                return false
            }
            if (prover.isUnsat) {
              logger.warn("Prover is already unsat when checked")
            }
            if (!addConstraint(!test))
              return false
            prover.isUnsat
        }
      }
    }.get.get
  }
}

case class InconsistentVariableEquality(
    v: SymbolicTerm[_],
    x: BigInt,
    y: BigInt,
) extends UserError {
  override def code: String = "inconsistentVariableEquality"
  override def text: String =
    s"Inconsistent variable equality: value of $v is required to be both $x and $y"
}

class ExpressionEqualityCheck[G](info: Option[AnnotationVariableInfo[G]]) {
  import ExpressionEqualityCheck._

  var replacerDepth = 0
  val max_depth = 100

  def usefulConditions(): Set[Expr[G]] = {
    info match {
      case None => Set()
      case Some(info) => info.usefulConditions
    }
  }

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
      case t: SymbolicTerm[G] if isValidSymbolicTerm(t) =>
        // Does it have a direct int value?
        info.flatMap(_.variableValues.get(t)).foreach(x => return Some(x))
        replaceSymbol(t).foreach(
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

    val normalBound = getBound(_, isLower)
    val reverseBound = getBound(_, !isLower)

    e match {
      case t: SymbolicTerm[G] if isValidSymbolicTerm(t) =>
        info.foreach { i =>
          if (isLower)
            i.lowerBound.get(t).foreach(b => return Some(b))
          else
            i.upperBound.get(t).foreach(b => return Some(b))
        }
        replaceSymbol(t) match {
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
      case Mod(e1, e2) => isConstantIntRecurse(e2).map(_.abs - 1)
      case TruncMod(e1, e2) if isLower =>
        lowerBoundRecurse(e1) match {
          case Some(l1) if l1 >= 0 => return Some(0)
          case _ => return isConstantIntRecurse(e2).map(i => -(i.abs - 1))
        }
      case TruncMod(e1, e2) => return isConstantIntRecurse(e2).map(_.abs - 1)
      case FloorDiv(e1, e2) if isConstantIntRecurse(e2).isDefined =>
        val divisor = isConstantIntRecurse(e2).get
        if (divisor == 0)
          return None
        else if (divisor > 0) {
          normalBound(e1) match {
            case Some(num) if num >= 0 => return Some(num / divisor)
            case Some(num) if num < 0 =>
              // So we have something like -10/3 (FloorDiv = Eucledian division) The lower bound for this is: -4
              // Since -10/3 gives back -3 in scala (uses truncated division) we should fix that
              // So if num % divisor != 0, we have to subtract one extra
              return Some(
                num / divisor -
                  (if (num % divisor != 0)
                     1
                   else
                     0)
              )
            case None =>
          }
        } else if (divisor > 0) {
          // dividing by negative number reverses the bound
          reverseBound(e1) match {
            case Some(num) if num >= 0 =>
              // This goes alright, since something like 10/-3 is the same for both eucledian as truncated div
              return Some(num / divisor)
            case Some(num) if num < 0 =>
              // So we have something like -10/-3 (FloorDiv = Eucledian division) The lower bound for this is: 4
              // Since -10/-3 gives back 3 in scala (uses truncated division) we should fix that
              // So if num % divisor != 0, we have to add one extra
              return Some(
                num / divisor +
                  (if (num % divisor != 0)
                     1
                   else
                     0)
              )
            case None =>
          }
        }
      case _ =>
    }

    for (c <- usefulConditions()) {
      c match {
        case Less(left, right) if isLower && right == e =>
          isConstantIntRecurse(left).map(l => return Some(l + 1))
        case LessEq(left, right) if isLower && right == e =>
          isConstantIntRecurse(left).map(l => return Some(l))
        case Greater(left, right) if isLower && left == e =>
          isConstantIntRecurse(right).map(l => return Some(l + 1))
        case GreaterEq(left, right) if isLower && left == e =>
          isConstantIntRecurse(right).map(l => return Some(l))

        case Less(left, right) if !isLower && left == e =>
          isConstantIntRecurse(right).map(l => return Some(l + 1))
        case LessEq(left, right) if !isLower && left == e =>
          isConstantIntRecurse(right).map(l => return Some(l))
        case Greater(left, right) if !isLower && right == e =>
          isConstantIntRecurse(left).map(l => return Some(l + 1))
        case GreaterEq(left, right) if !isLower && right == e =>
          isConstantIntRecurse(left).map(l => return Some(l))

        case Eq(left, right) if left == e =>
          isConstantIntRecurse(right).map(l => return Some(l))
        case Eq(left, right) if right == e =>
          isConstantIntRecurse(left).map(l => return Some(l))
        case _ =>
      }
    }

    None
  }

  def lessThenEq(lhs: Expr[G], rhs: Expr[G]): Option[Boolean] = {
    // Only defined for integer types for now
    (lhs.t, rhs.t) match {
      case (_: IntType[G], _: IntType[G]) =>
      case _ => return None
    }
    replacerDepth = 0
    lessThenEqRecurse(lhs, rhs)
  }

  // Compare factor*lhs <= factor*rhs
  private def compareMult(
      lhs: Expr[G],
      rhs: Expr[G],
      factor: Expr[G],
  ): Option[Boolean] = {
    if (!isNonZeroRecurse(factor).getOrElse(false))
      return None
    getSignRecurse(factor) match {
      case Some(Pos()) => lessThenEqRecurse(lhs, rhs)
      case Some(Neg()) => lessThenEqRecurse(rhs, lhs)
      case _ => None
    }
  }

  private def lessThenEqRecurse(lhs: Expr[G], rhs: Expr[G]): Option[Boolean] = {
    (isConstantIntRecurse(lhs), isConstantIntRecurse(rhs)) match {
      // Compare values directly
      case (Some(i1), Some(i2)) => return Some(i1 <= i2)
      case _ =>
    }
    // Compare two variables, where we sometimes store information about
    (lhs, rhs) match {
      case (v1: SymbolicTerm[G], v2: SymbolicTerm[G])
          if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) =>
        info.foreach { i =>
          if (
            i.lessThanEqVars.contains(v1) && i.lessThanEqVars(v1).contains(v2)
          )
            return Some(true)
        }
      case (left, Mult(r1, r2)) if equalExpressionsRecurse(left, r1) =>
        compareMult(const(1)(left.o), r2, left).foreach(x => return Some(x))
      case (left, Mult(r1, r2)) if equalExpressionsRecurse(left, r2) =>
        compareMult(const(1)(left.o), r1, left).foreach(x => return Some(x))

      case (Mult(l1, l2), right) if equalExpressionsRecurse(l1, right) =>
        compareMult(l2, const(1)(right.o), right).foreach(x => return Some(x))
      case (Mult(l1, l2), right) if equalExpressionsRecurse(l2, right) =>
        compareMult(l1, const(1)(right.o), right).foreach(x => return Some(x))

      case (Mult(l1, l2), Mult(r1, r2)) if equalExpressionsRecurse(l1, r1) =>
        compareMult(l2, r2, l1).foreach(x => return Some(x))
      case (Mult(l1, l2), Mult(r1, r2)) if equalExpressionsRecurse(l1, r2) =>
        compareMult(l2, r1, l1).foreach(x => return Some(x))
      case (Mult(l1, l2), Mult(r1, r2)) if equalExpressionsRecurse(l2, r2) =>
        compareMult(l1, r1, l2).foreach(x => return Some(x))

      case (left, Mult(r1, r2)) if equalExpressionsRecurse(left, r2) =>
        val isPos = lessThenEqRecurse(const(0)(rhs.o), r1)
        isPos.foreach(r => return Some(r))
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
      case v: SymbolicTerm[G] if info.exists(_.variableNotZero.contains(v)) =>
        return Some(true)
      case v: SymbolicTerm[G] if isValidSymbolicTerm(v) =>
        replaceSymbol(v) match {
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
    (getSignRecurse(e1), getSignRecurse(e2)) match {
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
      rest_e1.map(getSignRecurse).foldLeft(true)({
        case (_, None) => return None
        case (p, Some(Pos())) => p
        case (p, Some(Neg())) => !p
      })

    val polarity2 =
      rest_e2.map(getSignRecurse).foldLeft(true)({
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
    replacerDepth = 0;
    getSignRecurse(e)
  }

  private def getSignRecurse(e: Expr[G]): Option[Sign] = {
    isConstantIntRecurse(e).map(i => isPos(i >= 0)) orElse lowerBoundRecurse(e)
      .flatMap(i =>
        if (i >= 0)
          Some(Pos())
        else
          None
      ) orElse upperBoundRecurse(e).flatMap(i =>
      if (i < 0)
        Some(Neg())
      else
        None
    ) orElse lessThenEqRecurse(const(0)(e.o), e).map(isPos)
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
      case (name1: SymbolicTerm[G], name2: SymbolicTerm[G])
          if isValidSymbolicTerm(name1) && isValidSymbolicTerm(name2) =>
        // TODO: This check is too strict for the invocations since the givenMap might be in any order
        if (lhs == rhs)
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
      case (name1: SymbolicTerm[G], e2) if isValidSymbolicTerm(name1) =>
        replaceSymbol(name1) match {
          case Some(es) => es.exists(e => equalExpressionsRecurse(e, e2))
          case None => false
        }
      case (e1, name2: SymbolicTerm[G]) if isValidSymbolicTerm(name2) =>
        replaceSymbol(name2) match {
          case Some(es) => es.exists(e => equalExpressionsRecurse(e1, e))
          case None => false
        }
      case (inv: MethodInvocation[G], _) if !inv.ref.decl.pure => false
      case (_, inv: MethodInvocation[G]) if !inv.ref.decl.pure => false

      // In the general case, we are just interested in syntactic equality
      case (e1, e2) => e1 == e2
    }
  }

  def replaceSymbol(name: SymbolicTerm[G]): Option[Set[Expr[G]]] = {
    if (replacerDepth > max_depth) { return None }
    info.map(_.variableEqualities).flatMap(_.get(name).map(x => {
      replacerDepth += 1; x
    }))
  }
}

case class AnnotationVariableInfo[G](
    variableEqualities: Map[SymbolicTerm[G], Set[Expr[G]]],
    variableValues: Map[SymbolicTerm[G], BigInt],
    variableSynonyms: Map[SymbolicTerm[G], BigInt],
    variableNotZero: Set[SymbolicTerm[G]],
    lessThanEqVars: Map[SymbolicTerm[G], Set[SymbolicTerm[G]]],
    upperBound: Map[SymbolicTerm[G], BigInt],
    lowerBound: Map[SymbolicTerm[G], BigInt],
    usefulConditions: Set[Expr[G]],
)

/** This class gathers information about variables, such as: `requires x == 0`
  * and stores that x is equal to the value 0. Which we can use in simplify
  * steps This information is returned with ```getInfo(annotations:
  * Iterable[Expr[G]])```
  */
class AnnotationVariableInfoGetter[G](
    val variableEqualities: mutable.Map[SymbolicTerm[G], mutable.ListBuffer[
      Expr[G]
    ]],
    val variableValues: mutable.Map[SymbolicTerm[G], BigInt],
    // We put synonyms in the same group and give them a group number, to identify the same synonym groups
    val variableSynonyms: mutable.Map[SymbolicTerm[G], BigInt],
    val variableNotZero: mutable.Set[SymbolicTerm[G]],
    val lessThanEqVars: mutable.Map[SymbolicTerm[G], mutable.Set[SymbolicTerm[
      G
    ]]],
    // upperBound(v) = 5 Captures that variable v is less than or equal to 5
    val upperBound: mutable.Map[SymbolicTerm[G], BigInt],
    // lowerBound(v) = 5 Captures that variable v is greater than or equal to 5
    val lowerBound: mutable.Map[SymbolicTerm[G], BigInt],
    val usefulConditions: mutable.ArrayBuffer[Expr[G]],
) {

  def this() = {
    this(
      mutable.Map[SymbolicTerm[G], mutable.ListBuffer[Expr[G]]](),
      mutable.Map[SymbolicTerm[G], BigInt](),
      mutable.Map[SymbolicTerm[G], BigInt](),
      mutable.Set[SymbolicTerm[G]](),
      mutable.Map[SymbolicTerm[G], mutable.Set[SymbolicTerm[G]]](),
      mutable.Map[SymbolicTerm[G], BigInt](),
      mutable.Map[SymbolicTerm[G], BigInt](),
      mutable.ArrayBuffer[Expr[G]](),
    )
  }
  var currentSynonymGroup = 0
  var equalCheck: ExpressionEqualityCheck[G] = ExpressionEqualityCheck()

  def extractEqualities(e: Expr[G]): Unit = {
    e match {
      case Eq(e1, e2) =>
        (e1, e2) match {
          case (v1: SymbolicTerm[G], v2: SymbolicTerm[G])
              if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) =>
            addSynonym(v1, v2)
          case (v1: SymbolicTerm[G], _) if isValidSymbolicTerm(v1) =>
            addName(v1, e2)
          case (_, v2: SymbolicTerm[G]) if isValidSymbolicTerm(v2) =>
            addName(v2, e1)
          case _ =>
        }
      case _ =>
    }
  }

  def addLessEq(v: SymbolicTerm[G], i: BigInt): Unit = {
    val value = upperBound.getOrElse(v, i).min(i)
    upperBound(v) = value
  }

  def addGreaterEq(v: SymbolicTerm[G], i: BigInt): Unit = {
    val value = lowerBound.getOrElse(v, i).max(i)
    lowerBound(v) = value
  }

  def lt(e1: Expr[G], e2: Expr[G], equal: Boolean): Unit = {
    e1 match {
      // x <= i
      case v1: SymbolicTerm[G] if equal && isValidSymbolicTerm(v1) =>
        equalCheck.isConstantInt(e2).foreach { i =>
          if (i < 0)
            variableNotZero.add(v1)
          addLessEq(v1, i)
        }
      // x < i
      case v1: SymbolicTerm[G] if !equal && isValidSymbolicTerm(v1) =>
        equalCheck.isConstantInt(e2).foreach { i =>
          if (i <= 0)
            variableNotZero.add(v1)
          addLessEq(v1, i - 1)
        }
      case _ =>
    }
    e2 match {
      // i <= x
      case v2: SymbolicTerm[G] if equal && isValidSymbolicTerm(v2) =>
        equalCheck.isConstantInt(e1).foreach { i =>
          if (i > 0)
            variableNotZero.add(v2)
          addGreaterEq(v2, i)
        }
      // i < x
      case v2: SymbolicTerm[G] if !equal && isValidSymbolicTerm(v2) =>
        equalCheck.isConstantInt(e1).foreach { i =>
          if (i >= 0)
            variableNotZero.add(v2)
          addGreaterEq(v2, i + 1)
        }
      case _ =>
    }

    (e1, e2) match {
      // x < y
      case (v1: SymbolicTerm[G], v2: SymbolicTerm[G])
          if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) =>
        lessThanEqVars.getOrElseUpdate(v1, mutable.Set()).addOne(v2)
      case _ =>
    }
  }

  // n == m + 1 then m <= n
  def varEqVarPlusInt(
      v1: SymbolicTerm[G],
      v2: SymbolicTerm[G],
      i: BigInt,
  ): Unit = {
    if (i >= 0)
      lessThanEqVars.getOrElseUpdate(v2, mutable.Set()).addOne(v1)
    if (i <= 0)
      lessThanEqVars.getOrElseUpdate(v1, mutable.Set()).addOne(v2)
  }

  // n == m + k,
  // if k>=0 then m <= n
  // if k<=0 then n <= m
  def varEqVarPlusVar(
      n: SymbolicTerm[G],
      m: SymbolicTerm[G],
      k: SymbolicTerm[G],
  ): Unit = {
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
      case e: UnExpr[G] => isSimpleExpr(e.arg)
      case t: SymbolicTerm[G] if isValidSymbolicTerm(t) => true
      case _: Constant[G] => true
      case _ => false
    }
  }

  def extractComparisons(e: Expr[G]): Unit = {
    e match {
      case Neq(e1, e2) =>
        e1 match {
          // x != 0
          case v1: SymbolicTerm[G] if isValidSymbolicTerm(v1) =>
            equalCheck.isConstantInt(e2).foreach { i =>
              if (i == 0)
                variableNotZero.add(v1)
            }
          case _ =>
        }
        e2 match {
          // 0 != x
          case v2: SymbolicTerm[G] if isValidSymbolicTerm(v2) =>
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
      case Eq(v1: SymbolicTerm[G], Plus(v2: SymbolicTerm[G], i: ConstantInt[G]))
          if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) =>
        varEqVarPlusInt(v1, v2, i.value)
      case Eq(v1: SymbolicTerm[G], Plus(i: ConstantInt[G], v2: SymbolicTerm[G]))
          if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) =>
        varEqVarPlusInt(v1, v2, i.value)
      case Eq(Plus(v2: SymbolicTerm[G], i: ConstantInt[G]), v1: SymbolicTerm[G])
          if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) =>
        varEqVarPlusInt(v1, v2, i.value)
      case Eq(Plus(i: ConstantInt[G], v2: SymbolicTerm[G]), v1: SymbolicTerm[G])
          if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) =>
        varEqVarPlusInt(v1, v2, i.value)
      case Eq(
            v1: SymbolicTerm[G],
            Plus(v2: SymbolicTerm[G], v3: SymbolicTerm[G]),
          )
          if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) &&
            isValidSymbolicTerm(v3) =>
        varEqVarPlusVar(v1, v2, v3)
      case Eq(
            Plus(v2: SymbolicTerm[G], v3: SymbolicTerm[G]),
            v1: SymbolicTerm[G],
          )
          if isValidSymbolicTerm(v1) && isValidSymbolicTerm(v2) &&
            isValidSymbolicTerm(v3) =>
        varEqVarPlusVar(v1, v2, v3)
      case _ =>
    }
  }

  def addSynonym(v1: SymbolicTerm[G], v2: SymbolicTerm[G]): Unit = {
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

  def addValue(v: SymbolicTerm[G], x: BigInt): Unit =
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

  def addName(v: SymbolicTerm[G], expr: Expr[G]): Unit = {
    // Add to constant list
    isConstantInt[G](expr) match {
      case Some(x) => addValue(v, x)
      case None =>
        val list = variableEqualities.getOrElseUpdate(v, mutable.ListBuffer())
        list.addOne(expr)
    }
  }

  def addInfo(annotation: Expr[G]): Unit = {
    var ann = annotation
    usefulConditions.foreach { c =>
      // If a known true condition appears, literally in the annotation replace it with true. Ideally this would be a
      // better check since we do not catch a condition `!c` which appears in the annotation as `c` This replacement
      // isn't necessary when using the SMT solver but not every check in SimplifyNestedQuantifiers uses an SMT solver.
      val simple = replaceWithTrueAndSimplify(ann, c)
      if (simple.isDefined) { ann = simple.get }
    }
    extractEqualities(ann)

    if (isSimpleExpr(ann)) {
      addSimpleAnnotation(ann)
      Seq.range(0, usefulConditions.length).foreach { i =>
        // See if we can redo with a simpler expression
        val simple = replaceWithTrueAndSimplify(usefulConditions(i), ann)
        if (simple.isDefined) {
          extractEqualities(simple.get)
          addSimpleAnnotation(simple.get)
          usefulConditions(i) = simple.get
        }
      }
      if (ann != tt[G]) { usefulConditions.addOne(ann) }
    }
  }

  private def replaceWithTrueAndSimplify(
      original: Expr[G],
      replacer: Expr[G],
  ): Option[Expr[G]] = {
    implicit val o: Origin = original.o
    original match {
      case it if it == replacer => Some(tt)
      case And(l, r) =>
        (
          replaceWithTrueAndSimplify(l, replacer),
          replaceWithTrueAndSimplify(r, replacer),
        ) match {
          case (Some(BooleanValue(true)), newR) => Some(newR.getOrElse(r))
          case (Some(BooleanValue(false)), _) => Some(ff)
          case (newL, Some(BooleanValue(true))) => Some(newL.getOrElse(l))
          case (_, Some(BooleanValue(false))) => Some(ff)
          case (Some(l), newR) => Some(l && newR.getOrElse(r))
          case (newL, Some(r)) => Some(newL.getOrElse(l) && r)
          case (None, None) => None
        }
      case Star(l, r) =>
        (
          replaceWithTrueAndSimplify(l, replacer),
          replaceWithTrueAndSimplify(r, replacer),
        ) match {
          case (Some(BooleanValue(true)), newR) => Some(newR.getOrElse(r))
          case (Some(BooleanValue(false)), _) => Some(ff)
          case (newL, Some(BooleanValue(true))) => Some(newL.getOrElse(l))
          case (_, Some(BooleanValue(false))) => Some(ff)
          case (Some(l), newR) => Some(l &* newR.getOrElse(r))
          case (newL, Some(r)) => Some(newL.getOrElse(l) &* r)
          case (None, None) => None
        }
      case Or(l, r) =>
        (
          replaceWithTrueAndSimplify(l, replacer),
          replaceWithTrueAndSimplify(r, replacer),
        ) match {
          case (Some(BooleanValue(true)), _) => Some(tt)
          case (Some(BooleanValue(false)), newR) => Some(newR.getOrElse(r))
          case (_, Some(BooleanValue(true))) => Some(tt)
          case (newL, Some(BooleanValue(false))) => Some(newL.getOrElse(l))
          case (Some(l), newR) => Some(l || newR.getOrElse(r))
          case (newL, Some(r)) => Some(newL.getOrElse(l) || r)
          case (None, None) => None
        }
      case Implies(l, r) =>
        (
          replaceWithTrueAndSimplify(l, replacer),
          replaceWithTrueAndSimplify(r, replacer),
        ) match {
          case (Some(BooleanValue(true)), newR) => Some(newR.getOrElse(r))
          case (Some(BooleanValue(false)), _) => Some(tt)
          case (_, Some(BooleanValue(true))) => Some(tt)
          case (newL, Some(BooleanValue(false))) => Some(!newL.getOrElse(l))
          case (Some(l), newR) => Some(l ==> newR.getOrElse(r))
          case (newL, Some(r)) => Some(newL.getOrElse(l) ==> r)
          case (None, None) => None
        }
      case Not(e) =>
        replaceWithTrueAndSimplify(e, replacer) match {
          case Some(BooleanValue(true)) => Some(ff)
          case Some(BooleanValue(false)) => Some(tt)
          case newE => newE.map(!_)
        }
      case _ => None
    }
  }

  private def addSimpleAnnotation(annotation: Expr[G]): Unit = {
    val res = AnnotationVariableInfo[G](
      variableEqualities.view.mapValues(_.toSet).toMap,
      variableValues.toMap,
      variableSynonyms.toMap,
      Set[SymbolicTerm[G]](),
      Map[SymbolicTerm[G], Set[SymbolicTerm[G]]](),
      Map[SymbolicTerm[G], BigInt](),
      Map[SymbolicTerm[G], BigInt](),
      usefulConditions.toSet,
    )

    equalCheck = ExpressionEqualityCheck(Some(res))
    extractComparisons(annotation)
  }

  def mergeIntMaps(
      maps: Seq[Map[Local[G], BigInt]],
      combineInt: (Local[G], BigInt, BigInt) => BigInt,
  ): Map[Local[G], BigInt] = {
    val grouped = maps.flatMap(_.toSeq).groupBy(_._1)
    grouped.map({ case (k, vs) =>
      (k, vs.map(_._2).reduce((l, r) => combineInt(k, l, r)))
    })
  }

  def mergeMaps[K, V](
      maps: Seq[mutable.Map[K, V]],
      combine: (K, V, V) => V,
  ): Map[K, V] = {
    val grouped = maps.flatMap(_.toSeq).groupBy(_._1)
    grouped.map({ case (k, vs) =>
      (k, vs.map(_._2).reduce((l, r) => combine(k, l, r)))
    })
  }

  def finalInfo(
      prev: Seq[AnnotationVariableInfoGetter[G]]
  ): AnnotationVariableInfo[G] = {
    // I've changed this to do the distribution later, that means I'm doing a bunch of copying first though
    // it would probably be possible to merge everything into this getter so that there isn't this much duplication
    val varEq = mutable.Map.newBuilder
      .addAll(mergeMaps[SymbolicTerm[G], mutable.ListBuffer[Expr[G]]](
        prev.map(_.variableEqualities) :+ variableEqualities,
        (_, l, r) => l ++ r,
      )).result()
    val varVal = mutable.Map.newBuilder
      .addAll(mergeMaps[SymbolicTerm[G], BigInt](
        prev.map(_.variableValues) :+ variableValues,
        (v, x, y) =>
          if (x != y)
            throw InconsistentVariableEquality(v, x, y)
          else
            x,
      )).result()
    val varSyn: mutable.Map[SymbolicTerm[G], BigInt] = mutable.Map()
    var groupNumber = 0
    for (p <- prev) {
      val mappingPrevNew: mutable.Map[BigInt, BigInt] = mutable.Map()
      for ((v, synonym_nr) <- p.variableSynonyms) {
        if (varSyn.contains(v)) {
          if (
            mappingPrevNew.contains(synonym_nr) &&
            mappingPrevNew(synonym_nr) != varSyn(v)
          ) {
            // Merge
            varSyn.mapValuesInPlace((_, group) =>
              if (group == mappingPrevNew(synonym_nr))
                varSyn(v)
              else
                group
            )
          }
          mappingPrevNew(synonym_nr) = varSyn(v)
        } else if (mappingPrevNew.contains(synonym_nr)) {
          varSyn(v) = mappingPrevNew(synonym_nr)
        } else {
          varSyn(v) = groupNumber
          mappingPrevNew(synonym_nr) = groupNumber
          groupNumber += 1
        }
      }
    }
    val mappingThisNew: mutable.Map[BigInt, BigInt] = mutable.Map()
    for ((v, synonym_nr) <- variableSynonyms) {
      if (varSyn.contains(v)) {
        if (
          mappingThisNew.contains(synonym_nr) &&
          mappingThisNew(synonym_nr) != varSyn(v)
        ) {
          // Merge
          varSyn.mapValuesInPlace((_, group) =>
            if (group == mappingThisNew(synonym_nr))
              varSyn(v)
            else
              group
          )
        }
        mappingThisNew(synonym_nr) = varSyn(v)
      } else if (mappingThisNew.contains(synonym_nr)) {
        varSyn(v) = mappingThisNew(synonym_nr)
      } else {
        varSyn(v) = groupNumber
        mappingThisNew(synonym_nr) = groupNumber
        groupNumber += 1
      }
    }
    val varNotZero = mutable.Set.newBuilder
      .addAll(prev.flatMap(_.variableNotZero) ++ variableNotZero).result()
    val varLessThen = mutable.Map.newBuilder
      .addAll(mergeMaps[SymbolicTerm[G], mutable.Set[SymbolicTerm[G]]](
        prev.map(_.lessThanEqVars) :+ lessThanEqVars,
        (_, l, r) => l ++ r,
      )).result()
    // Take the lowest upper bound
    val varUpper = mutable.Map.newBuilder
      .addAll(mergeMaps[SymbolicTerm[G], BigInt](
        prev.map(_.upperBound) :+ upperBound,
        (_, l, r) => l.min(r),
      )).result()
    // Take the highest lower bound
    val varLower = mutable.Map.newBuilder
      .addAll(mergeMaps[SymbolicTerm[G], BigInt](
        prev.map(_.lowerBound) :+ lowerBound,
        (_, l, r) => l.max(r),
      )).result()
    val useful: mutable.ArrayBuffer[Expr[G]] = mutable
      .ArrayBuffer((prev.flatMap(_.usefulConditions) ++ usefulConditions): _*)

    val getter =
      new AnnotationVariableInfoGetter[G](
        varEq,
        varVal,
        varSyn,
        varNotZero,
        varLessThen,
        varUpper,
        varLower,
        useful,
      )

    getter.distributeInfo()

    AnnotationVariableInfo(
      getter.variableEqualities.view.mapValues(_.toSet).toMap,
      getter.variableValues.toMap,
      getter.variableSynonyms.toMap,
      getter.variableNotZero.toSet,
      getter.lessThanEqVars.view.mapValues(_.toSet).toMap,
      getter.upperBound.toMap,
      getter.lowerBound.toMap,
      getter.usefulConditions.toSet,
    )
  }

  override def clone(): AnnotationVariableInfoGetter[G] = {
    val res =
      new AnnotationVariableInfoGetter[G](
        variableEqualities.clone(),
        variableValues.clone(),
        variableSynonyms.clone(),
        variableNotZero.clone(),
        lessThanEqVars.clone(),
        upperBound.clone(),
        lowerBound.clone(),
        usefulConditions.clone(),
      )
    res
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
    usefulConditions.clear()
  }

  def filterInfo(assignedVars: Set[SymbolicTerm[G]]): Unit = {
    def const: Expr[G] => Boolean = isConstant(_, assignedVars)
    def constantVar(l: SymbolicTerm[G]): Boolean =
      constantType(l.t) && !assignedVars.contains(l)

    variableEqualities
      .filterInPlace((l, v) => constantVar(l) && v.forall(const))
    variableValues.filterInPlace((l, _) => constantVar(l))
    variableSynonyms.filterInPlace((l, _) => constantVar(l))
    variableNotZero.filterInPlace(constantVar)
    lessThanEqVars.filterInPlace((l, _) => constantVar(l))
    lessThanEqVars.mapValuesInPlace((_, eqs) => eqs.filterInPlace(constantVar))
    upperBound.filterInPlace((l, _) => constantVar(l))
    lowerBound.filterInPlace((l, _) => constantVar(l))
    usefulConditions.filterInPlace(const)
  }

  def isConstant(e: Expr[G], assignedVars: Set[SymbolicTerm[G]]): Boolean = {
    def rec(e: Expr[G]) = isConstant(e, assignedVars)
    // No need to check if the found SymbolicTerms are valid here since this is only ever called on expression containing previously checked terms
    e match {
      case inv: AnyFunctionInvocation[G] =>
        inv.args.forall(rec) && inv.givenMap.map(_._2).forall(rec) &&
        inv.yields.map(_._1).forall(rec)
      case t: SymbolicTerm[G]
          if constantType(t.t) && !assignedVars.contains(t) =>
        true
      case _: Constant[G] => true
      case e: UnExpr[G] => rec(e.arg)
      case e: BinExpr[G] => rec(e.left) && rec(e.right)
      case _ => false
    }
  }

  // Types which contain values not stored on the heap
  def constantType(t: Type[G]): Boolean =
    t match {
      case _: PrimitiveType[G] => true
      case c: CompositeType[G] => c.subtypes.forall(constantType)
      case TUnion(ts) => ts.forall(constantType)
      case _ => false
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
    val synonymSets: mutable.Map[BigInt, mutable.Set[SymbolicTerm[G]]] = mutable
      .Map()
    variableSynonyms.foreach { case (v, groupId) =>
      synonymSets.getOrElseUpdate(groupId, mutable.Set()).add(v)
    }

    def hasValue(vars: mutable.Set[SymbolicTerm[G]]): Option[BigInt] = {
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
      val greaterVars: mutable.Set[SymbolicTerm[G]] = mutable.Set()
      vars.foreach { v =>
        lessThanEqVars.get(v).foreach(ltSet =>
          ltSet.foreach(lt =>
            variableSynonyms.get(lt).foreach { i =>
              greaterVars.addAll(synonymSets(i))
            }
          )
        )
      }
      // Redistribute all greater vars again
      vars.foreach { lessThanEqVars.get(_).foreach(_.addAll(greaterVars)) }

    }
  }

}
