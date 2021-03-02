package vct.col.rewrite.gpgpuoptimizations

import java.util

import hre.lang.System.Progress
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, Type}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.expr.{NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTSpecial, Contract, DeclarationStatement, GPUOptName, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder, NameScanner}
import vct.col.features
import vct.col.features.Feature
import vct.col.rewrite.SilverClassReduction
import vct.col.util.AbstractTypeCheck
import vct.logging.PassReport
import vct.main.AbstractPass
import vct.silver.ErrorDisplayVisitor

import scala.collection.JavaConverters._
import scala.language.postfixOps

//TODO OS extract the common methods for loop unrolling and iteration merging into an object.
//TODO OS remove all the Progress calls so the user does not see it.
case class LoopUnroll(override val source: ProgramUnit) extends AbstractRewriter(source) {

  override def rewriteAll(): ProgramUnit = super.rewriteAll()

  private var inLoop: Boolean = false

  def findUpdateStatement(s: LoopStatement, itervar: ASTNode): Option[(StandardOperator, ASTNode)] = {
    val visitor = FindUpdateStatement(null, itervar)
    val body = visitor.rewrite(s.getBody)
    if (s.getUpdateBlock != null) s.getUpdateBlock.apply(visitor)

    visitor.updateStmnt
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

  def findBounds(s: LoopStatement, K: Int, C: Int, itervar: ASTNode, updateStmnt: (StandardOperator, ASTNode)): Option[(ASTNode, ASTNode, Contract)] = {
    val cb = new ContractBuilder
    val invs = s.getContract.invariant
    var lowerbounds: Set[ASTNode] = Set.empty
    var upperbounds: Set[ASTNode] = Set.empty

    ASTUtils.conjuncts(invs, Star, And).forEach {
      case e: OperatorExpression => e.operator match {
        case LT if e.second.equals(itervar) =>
          if (updateStmnt._1 == Plus) {
            val updatedLowerbound = create.expression(Plus, e.first, constant(C*K))
            val updatedInvariant = create.expression(LT, updatedLowerbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else if (updateStmnt._1 == Mult) {
            val updatedLowerbound = create.expression(Mult, e.first, constant(scala.math.pow(C,K).toInt))
            val updatedInvariant = create.expression(LT, updatedLowerbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else {
            cb.appendInvariant(rewrite(e))
          }

//          cb.appendInvariant(rewrite(e))
          lowerbounds ++= Set(create.expression(Plus, e.first, create.constant(1))) // Lowerbound
        case LTE if e.second.equals(itervar) =>
//          C <= i
          if (updateStmnt._1 == Plus) {
            val updatedLowerbound = create.expression(Plus, e.first, constant(C*K))
            val updatedInvariant = create.expression(LTE, updatedLowerbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else if (updateStmnt._1 == Mult) {
            val updatedLowerbound = create.expression(Mult, e.first, constant(scala.math.pow(C,K).toInt))
            val updatedInvariant = create.expression(LTE, updatedLowerbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else {
            cb.appendInvariant(rewrite(e))
          }
          lowerbounds ++= Set(e.first) // Lowerbound

        case GT if e.first.equals(itervar) =>
//          i > 0
          if (updateStmnt._1 == Plus) {
            val updatedLowerbound = create.expression(Plus, e.second, constant(C*K))
            val updatedInvariant = create.expression(GT, rewrite(e.first), updatedLowerbound)
            cb.appendInvariant(updatedInvariant)
          } else if (updateStmnt._1 == Mult) {
            val updatedLowerbound = create.expression(Mult, e.second, constant(scala.math.pow(C,K).toInt))
            val updatedInvariant = create.expression(GT, rewrite(e.second), updatedLowerbound)
            cb.appendInvariant(updatedInvariant)
          } else {
            cb.appendInvariant(rewrite(e))
          }
          lowerbounds ++= Set(create.expression(Minus, e.second, create.constant(1))) // Lowerbound
        case GTE if e.first.equals(itervar) =>
          if (updateStmnt._1 == Plus) {
            val updatedLowerbound = create.expression(Plus, e.second, constant(C*K))
            val updatedInvariant = create.expression(GTE, rewrite(e.first), updatedLowerbound)
            cb.appendInvariant(updatedInvariant)
          } else if (updateStmnt._1 == Mult) {
            val updatedLowerbound = create.expression(Mult, e.second, constant(scala.math.pow(C,K).toInt))
            val updatedInvariant = create.expression(GTE, rewrite(e.second), updatedLowerbound)
            cb.appendInvariant(updatedInvariant)
          } else {
            cb.appendInvariant(rewrite(e))
          }
          lowerbounds ++= Set(e.second) // Lowerbound



        case GT if e.second.equals(itervar) =>
//         b > i
          if (updateStmnt._1 == Minus) {
            val updatedUpperbound = create.expression(Minus, e.first, constant(C*K))
            val updatedInvariant = create.expression(GT, updatedUpperbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else if (updateStmnt._1 == FloorDiv) {
            val updatedUpperbound = create.expression(FloorDiv, e.first, constant(scala.math.pow(C,K).toInt))
            val updatedInvariant = create.expression(GT, updatedUpperbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else {
            cb.appendInvariant(rewrite(e))
          }
          upperbounds ++= Set(create.expression(Plus, e.first, create.constant(1))) // Upperbound
        case GTE if e.second.equals(itervar) =>
          if (updateStmnt._1 == Minus) {
            val updatedUpperbound = create.expression(Minus, e.first, constant(C*K))
            val updatedInvariant = create.expression(GTE, updatedUpperbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else if (updateStmnt._1 == FloorDiv) {
            val updatedUpperbound = create.expression(FloorDiv, e.first, constant(scala.math.pow(C,K).toInt))
            val updatedInvariant = create.expression(GTE, updatedUpperbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else {
            cb.appendInvariant(rewrite(e))
          }
          upperbounds ++= Set(e.first) // upperbound
        case LT if e.first.equals(itervar) =>
//         i < b
          if (updateStmnt._1 == Minus) {
            val updatedUpperbound = create.expression(Minus, e.second, constant(C*K))
            val updatedInvariant = create.expression(LT, rewrite(e.first), updatedUpperbound)
            cb.appendInvariant(updatedInvariant)
          } else if (updateStmnt._1 == FloorDiv) {
            val updatedUpperbound = create.expression(FloorDiv, e.second, constant(scala.math.pow(C,K).toInt))
            val updatedInvariant = create.expression(LT, rewrite(e.first), updatedUpperbound)
            cb.appendInvariant(updatedInvariant)
          } else {
            cb.appendInvariant(rewrite(e))
          }

          upperbounds ++= Set(create.expression(Minus, e.second, create.constant(1))) // Upperbound
        case LTE if e.first.equals(itervar) =>
          if (updateStmnt._1 == Minus) {
            val updatedUpperbound = create.expression(Minus, e.second, constant(C*K))
            val updatedInvariant = create.expression(LTE, rewrite(e.first), updatedUpperbound)
            cb.appendInvariant(updatedInvariant)
          } else if (updateStmnt._1 == FloorDiv) {
            val updatedUpperbound = create.expression(FloorDiv, e.second, constant(scala.math.pow(C,K).toInt))
            val updatedInvariant = create.expression(LTE, rewrite(e.first), updatedUpperbound)
            cb.appendInvariant(updatedInvariant)
          } else {
            cb.appendInvariant(rewrite(e))
          }
          upperbounds ++= Set(e.second) // Upperbound

        case _ => cb.appendInvariant(rewrite(e))
      }
      case default => cb.appendInvariant(rewrite(default))
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

    Option((a, b, cb.getContract(false)))
  }

  override def visit(s: LoopStatement): Unit = {
    if (s.getGpuopt == null || s.getGpuopt.name != GPUOptName.LoopUnroll) {
      super.visit(s)
      return
    } else if (inLoop) {
      //TODO OS think of a better warning message
      Warning("Only one loop can be optimized at a time. By default, the outer loop is optimized first. Please run VerCors again with the output to optimize the inner loops %s", s.getOrigin)
      super.visit(s)
      return
    }
    inLoop = true

    // The iteration variable, a NameExpression
    val itervar = s.getGpuopt.args.head
    // The number to unroll, an integer constant
    val K: Int = s.getGpuopt.args(1).asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value
    if (K <= 0) {
      Warning("Loop unroll optimization is not performed at %s", s.getGpuopt.getOrigin)
      return
    }

    //////////////////////////////////////////////
    /// check if condition matches our pattern ///
    //////////////////////////////////////////////
    val condCheck = matchCondition(s, itervar)

    if (!condCheck) {
      Fail("The condition of this loop does not match the pattern for this optimization", s.getEntryGuard.getOrigin)
      return
    }


    /////////////////////////////////
    /// find the update statement ///
    /////////////////////////////////
    val maybeUpdateStmt = findUpdateStatement(s, itervar)
    if (maybeUpdateStmt.isEmpty) {
      Fail("The iteration variable %s is never updated.", itervar)
    }

    val updateStmt = maybeUpdateStmt.get
    if (!updateStmt._2.isInstanceOf[ConstantExpression] || !updateStmt._2.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]) {
      Fail("%s in update statement is not a constant at ", updateStmt._2, updateStmt._2.getOrigin)
    }
    val C = updateStmt._2.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value
//    Progress("Update Statement: %s in", updateStmt)

    //////////////////////////////////////
    /// find the lower and upperbounds ///
    //////////////////////////////////////
    val maybeBounds = findBounds(s, K, C, itervar, updateStmt)
    if (maybeBounds.isEmpty) {
      //TODO OS, find the fail message that I wrote here.
    }
    val (a, b, contract) = maybeBounds.get

//    Progress("Lowerbound = %s", a)
//    Progress("Upperbound = %s", b)

    // TODO OS check if you can actually unroll



    ////////////////////////////////
    /// check if unroll is possible ///
    ////////////////////////////////
    //TODO OS how to get a free name
    val methodsStatic = true
    val nameOfU = "U" + (Math.random() * 100).asInstanceOf[Int]
    val nameOfInc = "inc" + (Math.random() * 100).asInstanceOf[Int]


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

    bodyOfCheck.add(create special(ASTSpecial.Kind.Assert, gt(size(invokeUForAssert), constant(K))))

    val checkMethodName = "check_loop_unroll_" + current_method.name
    val checkMethodContract = new ContractBuilder()
    checkMethodContract.requires(copy_rw.rewrite(current_method().getContract.pre_condition))
    checkMethodContract.requires(copy_rw.rewrite(current_method().getContract.invariant))

    val methodCheck = create.method_kind(
      current_method().kind,
      create.primitive_type(PrimitiveSort.Void),
      checkMethodContract.getContract(),
      checkMethodName,
      current_method().getArgs.map(d => copy_rw.rewrite(d)),
      bodyOfCheck
    )
    methodCheck.setStatic(methodsStatic)


    current_class().add_static(methodInc)
    current_class().add_static(methodU)
    current_class().add_static(methodCheck)

//    Progress("U: \n%s", methodU)
//    Progress("inc: \n%s", methodInc)
//    Progress("Check method: \n%s", methodCheck)



    ////////////////////////////////
    /// Start the loop unrolling ///
    ////////////////////////////////
    s.getBody match {
      case b: BlockStatement =>
        b.getStatements.foreach(st => current_sequence().add(copy_rw.rewrite(st)))
      case _ =>
        current_sequence().add(copy_rw.rewrite(s.getBody))
    }
    if (s.getUpdateBlock != null)
      s.getUpdateBlock match {
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

    if (s.getUpdateBlock != null)
      s.getUpdateBlock match {
        case b: BlockStatement =>
          b.forEachStmt(st => newbody.add(removeDecl.rewrite(st)))
        case notablock =>
          newbody.add(removeDecl.rewrite(notablock))
      }

    1 until K foreach {
      i => newbody.forEachStmt(stmt => current_sequence().add(copy_rw.rewrite(stmt)))
    }

    result = create.loop(
      rewrite(s.getInitBlock),
      rewrite(s.getEntryGuard),
      rewrite(s.getExitGuard),
      rewrite(s.getUpdateBlock),
      removeDecl.rewrite(s.getBody),
      contract
    )

    // At this point we can generate the method to see if unrolling is possible.

    //////////////////////
    // Unroll or return //
    //////////////////////
    inLoop = false
  }

}
