package vct.col.rewrite.gpgpuoptimizations

import java.util

import hre.lang.System.Fail
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, Type}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.expr.{NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTSpecial, Contract, DeclarationStatement, GPUOpt, LoopUnrolling, Method, ProgramUnit}
import vct.col.ast.util.{ASTFactory, ASTUtils, AbstractRewriter, ContractBuilder, NameScanner}
import vct.col.rewrite.gpgpuoptimizations.LoopOperations.{findUpdateStatement, matchCondition}
import vct.col.rewrite.gpgpuoptimizations.UnrollLoops.unrolledProgram

import scala.collection.mutable.Seq
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.postfixOps

//TODO OS how to get a free name (for future)

object UnrollLoops {
  var unrolledProgram: ProgramUnit = null
}

object LoopOperations {
  def findUpdateStatement(s: LoopStatement, itervar: ASTNode): (StandardOperator, ASTNode) = {
    val visitor = FindUpdateStatement(null, itervar)
    val body = visitor.rewrite(s.getBody)
    if (s.getUpdateBlock != null) s.getUpdateBlock.apply(visitor)

    if (visitor.updateStmnt.isEmpty) {
      Fail("The iteration variable %s is never updated.", itervar)
    }
    visitor.updateStmnt.get
  }

  def matchCondition(s: LoopStatement, itervar: ASTNode): Boolean = {
    val cond = s.getEntryGuard
    val condCheck: Boolean = cond match {
      case e: OperatorExpression => e.operator match {
        case LT => e.first.equals(itervar) ^ e.second.equals(itervar)
        case LTE => e.first.equals(itervar) ^ e.second.equals(itervar)
        case GT => e.first.equals(itervar) ^ e.second.equals(itervar)
        case GTE => e.first.equals(itervar) ^ e.second.equals(itervar)
        case EQ => e.first.equals(itervar) ^ e.second.equals(itervar)
        case NEQ => e.first.equals(itervar) ^ e.second.equals(itervar)
        case _ =>
          Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
          false
      }
      case _ =>
        Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
        false // Needed because Fail is not a Nothing.
    }
    condCheck
  }

  def findBounds(s: LoopStatement, itervar: ASTNode, create: ASTFactory[_]): (ASTNode, ASTNode) = {
    val invs = s.getContract.invariant
    var lowerbounds: Set[ASTNode] = Set.empty
    var upperbounds: Set[ASTNode] = Set.empty

    ASTUtils.conjuncts(invs, Star, And).forEach {
      case e: OperatorExpression => e.operator match {
        // Lowerbounds
        case LT if e.second.equals(itervar) =>
          lowerbounds ++= Set(create.expression(Plus, e.first, create.constant(1)))
        case LTE if e.second.equals(itervar) =>
          lowerbounds ++= Set(e.first)
        case GT if e.first.equals(itervar) =>
          lowerbounds ++= Set(create.expression(Minus, e.second, create.constant(1)))
        case GTE if e.first.equals(itervar) =>
          lowerbounds ++= Set(e.second)

        // Upperbounds
        case GT if e.second.equals(itervar) =>
          upperbounds ++= Set(create.expression(Plus, e.first, create.constant(1)))
        case GTE if e.second.equals(itervar) =>
          upperbounds ++= Set(e.first)
        case LT if e.first.equals(itervar) =>
          upperbounds ++= Set(create.expression(Minus, e.second, create.constant(1)))
        case LTE if e.first.equals(itervar) =>
          upperbounds ++= Set(e.second)
        case _ =>
      }
      case _ =>
    }

    if (lowerbounds.size > 1) {
      Fail("Multiple matches for the specification of the lowerbound for the loop variable %s %s.", itervar, lowerbounds)
    } else if (lowerbounds.isEmpty) {
      Fail("No matches for the specification of the lowerbound for the loop variable %s.", itervar)
    } else if (upperbounds.size > 1) {
      Fail("Multiple matches for the specification of the upperbound for the loop variable %s %s.", itervar, upperbounds)
    } else if (upperbounds.isEmpty) {
      Fail("No matches for the specification of the upperbound for the loop variable %s.", itervar)
    }

    // a is the lowerbound, b is the upperbound
    val a = lowerbounds.head
    val b = upperbounds.head

    (a, b)
  }
}

case class UnrollLoops(override val source: ProgramUnit, generateCheck: Boolean = true) extends AbstractRewriter(source) {

  private var inLoop: Boolean = false
  val methodsWithUnroll: mutable.Map[String, mutable.Buffer[Method]] = mutable.Map.empty.withDefaultValue(mutable.Buffer.empty)


  override def rewriteAll(): ProgramUnit = {
    val res = super.rewriteAll()

    if (generateCheck) {
      val targetWithChecks = RemoveBodies(source, methodsWithUnroll).rewriteAll()
      unrolledProgram = res
      targetWithChecks
    } else {
      res
    }
  }

  case class RemoveBodies(override val source: ProgramUnit, methodsToReplace: mutable.Map[String, mutable.Buffer[Method]]) extends AbstractRewriter(source) {
    override def visit(m: Method): Unit = {
      if (methodsToReplace(m.name).isEmpty) {
        m.setBody(null)
        super.visit(m)
      } else {
        methodsToReplace(m.name).foreach(currentTargetClass.add(_))
      }
    }
  }

  override def visit(s: LoopStatement): Unit = {
    if (s.getGpuopt == null || !s.getGpuopt.isInstanceOf[LoopUnrolling]) {
      super.visit(s)
      return
    } else if (inLoop) {
      //TODO OS think of a better warning message
      Warning("Only one loop can be optimized at a time. By default, the outer loop is optimized first. Please run VerCors again with the output to optimize the inner loops %s", s.getOrigin)
      super.visit(s)
      return
    }
    inLoop = true

    val lu = s.getGpuopt.asInstanceOf[LoopUnrolling]
    // itervar is the iteration variable and K (an integer constant) number to unroll,
    val itervar = lu.itervar
    val K: Int = lu.getK

    if (K <= 0) {
      Warning("K is smaller or equal to zero. Loop unroll optimization is not performed at %s", lu.getOrigin)
      return
    }

    // check if condition matches our pattern
    if (!matchCondition(s, itervar)) {
      Fail("The condition of this loop does not match the pattern for this optimization", s.getEntryGuard.getOrigin)
      return
    }

    // find the update statement
    val updateStmt = findUpdateStatement(s, itervar)
    if (!updateStmt._2.isInstanceOf[ConstantExpression] || !updateStmt._2.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]) {
      Fail("%s in update statement is not a constant at ", updateStmt._2, updateStmt._2.getOrigin)
    }

    // find the lower and upperbounds //
    // the invokation of findBounds is needed to check whether the bounds are there. //
    LoopOperations.findBounds(s, itervar, create)

    // Transform
    val contract = transformBoundsInLoopContract(s, K, itervar, updateStmt)
    // check if unroll is possible
    if (generateCheck)
      methodsWithUnroll(current_method().name) = methodsWithUnroll(current_method().name) ++ generateCheckMethods(s, itervar, K, updateStmt)
    // With all the necessary information gathered/transformed
    //    Start the loop unrolling
    val newLoop: LoopStatement = unrollLoop(s, itervar, K, contract)
    result = newLoop

    inLoop = false
  }

  def transformBoundsInLoopContract(s: LoopStatement, K: Int, itervar: ASTNode, updateStmnt: (StandardOperator, ASTNode)): Contract = {
    val cb = new ContractBuilder
    val op = updateStmnt._1
    val C = updateStmnt._2.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value

    ASTUtils.conjuncts(s.getContract.invariant, Star, And).forEach {
      case e: OperatorExpression => e.operator match {
        case LT if e.second.equals(itervar) =>
          val invariant = op match {
            case Plus =>
              val updatedLowerbound = create.expression(Plus, e.first, constant(C * K))
              create.expression(LT, updatedLowerbound, rewrite(e.second))
            case Mult =>
              val updatedLowerbound = create.expression(Mult, e.first, constant(scala.math.pow(C, K).toInt))
              create.expression(LT, updatedLowerbound, rewrite(e.second))
            case _ => rewrite(e)
          }
          cb.appendInvariant(invariant)
        case LTE if e.second.equals(itervar) =>
          val invariant = op match {
            case Plus =>
              val updatedLowerbound = create.expression(Plus, e.first, constant(C * K))
              create.expression(LTE, updatedLowerbound, rewrite(e.second))
            case Mult =>
              val updatedLowerbound = create.expression(Mult, e.first, constant(scala.math.pow(C, K).toInt))
              create.expression(LTE, updatedLowerbound, rewrite(e.second))
            case _ => rewrite(e)
          }
          cb.appendInvariant(invariant)

        case GT if e.first.equals(itervar) =>
          val invariant = op match {
            case Plus =>
              val updatedLowerbound = create.expression(Plus, e.second, constant(C * K))
              create.expression(GT, rewrite(e.first), updatedLowerbound)
            case Mult =>
              val updatedLowerbound = create.expression(Mult, e.second, constant(scala.math.pow(C, K).toInt))
              create.expression(GT, rewrite(e.second), updatedLowerbound)
            case _ => rewrite(e)
          }
          cb.appendInvariant(invariant)

        case GTE if e.first.equals(itervar) =>
          val invariant = op match {
            case Plus =>
              val updatedLowerbound = create.expression(Plus, e.second, constant(C * K))
              create.expression(GTE, rewrite(e.first), updatedLowerbound)
            case Mult =>
              val updatedLowerbound = create.expression(Mult, e.second, constant(scala.math.pow(C, K).toInt))
              create.expression(GTE, rewrite(e.second), updatedLowerbound)
            case _ => rewrite(e)
          }
          cb.appendInvariant(invariant)

        case GT if e.second.equals(itervar) =>
          val invariant = op match {
            case Minus =>
              val updatedUpperbound = create.expression(Minus, e.first, constant(C * K))
              create.expression(GT, updatedUpperbound, rewrite(e.second))
            case FloorDiv =>
              val updatedUpperbound = create.expression(FloorDiv, e.first, constant(scala.math.pow(C, K).toInt))
              create.expression(GT, updatedUpperbound, rewrite(e.second))
            case _ => rewrite(e)
          }
          cb.appendInvariant(invariant)

        case GTE if e.second.equals(itervar) =>
          val invariant = op match {
            case Minus =>
              val updatedUpperbound = create.expression(Minus, e.first, constant(C * K))
              create.expression(GTE, updatedUpperbound, rewrite(e.second))
            case FloorDiv =>
              val updatedUpperbound = create.expression(FloorDiv, e.first, constant(scala.math.pow(C, K).toInt))
              create.expression(GTE, updatedUpperbound, rewrite(e.second))
            case _ => rewrite(e)
          }
          cb.appendInvariant(invariant)

        case LT if e.first.equals(itervar) =>
          val invariant = op match {
            case Minus =>
              val updatedUpperbound = create.expression(Minus, e.second, constant(C * K))
              create.expression(LT, rewrite(e.first), updatedUpperbound)
            case FloorDiv =>
              val updatedUpperbound = create.expression(FloorDiv, e.second, constant(scala.math.pow(C, K).toInt))
              create.expression(LT, rewrite(e.first), updatedUpperbound)
            case _ => rewrite(e)
          }
          cb.appendInvariant(invariant)

        case LTE if e.first.equals(itervar) =>
          val invariant = op match {
            case Minus =>
              val updatedUpperbound = create.expression(Minus, e.second, constant(C * K))
              create.expression(LTE, rewrite(e.first), updatedUpperbound)

            case FloorDiv =>
              val updatedUpperbound = create.expression(FloorDiv, e.second, constant(scala.math.pow(C, K).toInt))
              create.expression(LTE, rewrite(e.first), updatedUpperbound)
            case _ => rewrite(e)
          }
          cb.appendInvariant(invariant)

        case _ => cb.appendInvariant(rewrite(e))
      }
      case default => cb.appendInvariant(rewrite(default))
    }

    cb.getContract(false)
  }

  private def unrollLoop(s: LoopStatement, itervar: NameExpression, K: Int, newContract: Contract) = {
    s.getInitBlock match {
      case null => //Do nothing
      case b: BlockStatement =>
        b.getStatements.foreach(st => current_sequence().add(rewrite(st)))
      case block =>
        current_sequence().add(rewrite(block))
    }

    ASTUtils.conjuncts(copy_rw.rewrite(s.getContract.invariant), Star, And).forEach {
        st => current_sequence().add(create special(ASTSpecial.Kind.Assert, copy_rw.rewrite(st)))
    }

    s.getBody match {
      case b: BlockStatement =>
        b.getStatements.foreach(st => current_sequence().add(copy_rw.rewrite(st)))
      case _ =>
        current_sequence().add(copy_rw.rewrite(s.getBody))
    }

    s.getUpdateBlock match {
      case null => //Do nothing
      case b: BlockStatement =>
        b.getStatements.foreach(st => current_sequence().add(rewrite(st)))
      case block =>
        current_sequence().add(rewrite(block))
    }

    // A copy of the new body (without any declarations)
    val newbody = create.block()
    val removeDecl = ReplaceDeclarationsByAssignments(null)

    s.getBody match {
      case b: BlockStatement =>
        b.getStatements.foreach(st => newbody.add(removeDecl.rewrite(st)))
      case _ =>
        newbody.add(removeDecl.rewrite(s.getBody))
    }

    s.getUpdateBlock match {
      case null => //Do nothing
      case b: BlockStatement =>
        b.forEachStmt(st => newbody.add(removeDecl.rewrite(st)))
      case notablock =>
        newbody.add(removeDecl.rewrite(notablock))
    }

    1 until K foreach {
      i =>
        ASTUtils.conjuncts(copy_rw.rewrite(s.getContract.invariant), Star, And).forEach {
          st =>
            current_sequence().add(create special(ASTSpecial.Kind.Assert, copy_rw.rewrite(st)))
        }
        newbody.forEachStmt(stmt => current_sequence().add(copy_rw.rewrite(stmt)))
    }

    val newLoop = create.loop(
      copy_rw.rewrite(itervar),
      rewrite(s.getEntryGuard),
      rewrite(s.getExitGuard),
      rewrite(s.getUpdateBlock),
      removeDecl.rewrite(s.getBody),
      newContract
    )
    newLoop
  }

  private def generateCheckMethods(s: LoopStatement, itervar: NameExpression, K: Int, updateStmt: (StandardOperator, ASTNode)): Seq[Method] = {
    val methodsStatic = true
    val nameOfU = "U" + (Math.random() * 100).asInstanceOf[Int]
    val nameOfInc = "update" + (Math.random() * 100).asInstanceOf[Int]


    val mapOfargsU = new util.LinkedHashMap[String, Type](NameScanner.freeVars(s.getEntryGuard))
    val argsOfU = genPars(mapOfargsU)

    val argsCallU = argsOfU.map(_.name).map(
      k => if (itervar.isName(k)) invoke(null, nameOfInc, rewrite(itervar)) else create.local_name(k)
    ).toSeq

    val invokeU = invoke(null, nameOfU, argsCallU: _*)

    val argsCallUForAssert = argsOfU.map(_.name).map(create.local_name).toSeq

    val invokeUForAssert = invoke(null, nameOfU, argsCallUForAssert: _*)


    val bodyOfU = create.expression(ITE,
      rewrite(s.getEntryGuard),
      concat(create.sequence(itervar.getType, itervar), invokeU),
      create.sequence(rewrite(itervar.getType), itervar)
    )
    // We put the freeVars in a linked hashmap to retain the order of insersion.


    val cb = new ContractBuilder()
    cb.ensures(gte(size(create.reserved_name(ASTReserved.Result)), constant(1)))
    cb.ensures(eq(get(create.reserved_name(ASTReserved.Result), constant(0)), rewrite(itervar)))
    cb.ensures(implies(
      rewrite(s.getEntryGuard),
      eq(size(create.reserved_name(ASTReserved.Result)), plus(size(invokeU), constant(1)))
    )
    )

    cb.ensures(implies(
      not(rewrite(s.getEntryGuard)),
      eq(size(create.reserved_name(ASTReserved.Result)), constant(1))
    )
    )


    val nameOfJ = "j" + (Math.random() * 100).asInstanceOf[Int]
    val forAllIndex = new DeclarationStatement(nameOfJ, create.primitive_type(PrimitiveSort.Integer))
    val indexNode = name(nameOfJ)

    val low = lte(constant(0), indexNode)
    val high = less(indexNode, minus(size(create.reserved_name(ASTReserved.Result)), constant(1)))


    cb.ensures(
      create.forall(
        and(low, high),
        eq(
          get(create.reserved_name(ASTReserved.Result), plus(indexNode, constant(1))),
          invoke(null, nameOfInc, get(create.reserved_name(ASTReserved.Result), indexNode)),
        ),
        forAllIndex
      )
    )

    val methodU = create.function_decl(
      create.primitive_type(PrimitiveSort.Sequence, create.primitive_type(PrimitiveSort.Integer)),
      cb.getContract(),
      nameOfU,
      argsOfU,
      bodyOfU
    )
    methodU.setStatic(methodsStatic)


    val methodInc = create.function_decl(
      create.primitive_type(PrimitiveSort.Integer),
      null,
      nameOfInc,
      Seq(new DeclarationStatement(itervar.asInstanceOf[NameExpression].name, create.primitive_type(PrimitiveSort.Integer))).asJava,
      create.expression(updateStmt._1, itervar, updateStmt._2)
    )
    methodInc.setStatic(methodsStatic)


    val bodyOfCheck = create.block()
    current_sequence().forEach(st => bodyOfCheck.add(copy_rw.rewrite(st)))
    s.getInitBlock match {
      case null => //Do nothing
      case b: BlockStatement =>
        b.getStatements.foreach(st => bodyOfCheck.add(rewrite(st)))
      case block =>
        bodyOfCheck.add(rewrite(block))
    }

    bodyOfCheck.add(create special(ASTSpecial.Kind.Assert, gt(size(invokeUForAssert), constant(K))))

    val checkMethodName = "check_loop_unroll_" + current_method.name
    val checkMethodContract = new ContractBuilder()
    checkMethodContract.requires(copy_rw.rewrite(current_method().getContract.pre_condition))
    checkMethodContract.requires(copy_rw.rewrite(current_method().getContract.invariant))

    val methodCheck = create.method_kind(
      //TODO OS this should be done differently, an errormapping has to be added to ErrorMap. The line below is for testing purposes
      //        new CompositeOrigin(new MessageOrigin("Could not prove unroll for this loop"), s.getGpuopt.getOrigin),
      current_method().kind,
      create.primitive_type(PrimitiveSort.Void),
      Array.empty[Type],
      checkMethodContract.getContract(),
      checkMethodName,
      current_method().getArgs.map(d => copy_rw.rewrite(d)),
      List.empty[GPUOpt].asJava,
      false,
      bodyOfCheck
    )
    methodCheck.setStatic(false)

    Seq(methodInc, methodU, methodCheck)
  }

}
