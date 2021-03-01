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

//TODO OS remove all the Progress calls so the user does not see it.
case class LoopUnroll(override val source: ProgramUnit) extends AbstractRewriter(source) {

  override def rewriteAll(): ProgramUnit = super.rewriteAll()

  case class ReplaceDeclarationsByAssignments(override val source: ProgramUnit) extends AbstractRewriter(source) {
    override def visit(s: DeclarationStatement): Unit = {
      result = create assignment(create local_name s.name, rewrite(s.init.getOrElse(rewrite(s.`type`).zero)))
    }
  }

  case class FindUpdateStatement(override val source: ProgramUnit, itervar: ASTNode) extends AbstractRewriter(source) {

    var updateStmnt: Option[(StandardOperator, ASTNode)] = None

    override def visit(o: OperatorExpression): Unit = {
      if (!o.first.equals(itervar) || !Set(AddAssign, PostIncr, PostDecr, SubAssign, MulAssign, DivAssign).contains(o.operator)) {
        super.visit(o)
        return
      } else if (updateStmnt.isDefined) {
        Fail("Multiple update statements for iteration variable %s. Only one update statement is allowed.", itervar)
      }

      o.operator match {
        //TODO OS add cases for i += -C (and for -=, *=, /=)
        case AddAssign =>
          updateStmnt = Some(Plus, o.second)
        case PostIncr =>
          updateStmnt = Some(Plus, create constant 1)
        case PostDecr =>
          updateStmnt = Some(Minus, create constant 1)
        case SubAssign =>
          updateStmnt = Some(Minus, o.second)
        case MulAssign =>
          updateStmnt = Some(Mult, o.second)
        case DivAssign =>
          updateStmnt = Some(Div, o.second)
        case _ =>
      }

      super.visit(o)
    }

    override def visit(s: AssignmentStatement): Unit = {
      if (!s.location.equals(itervar)) {
        super.visit(s)
        return
      } else if (updateStmnt.isDefined) {
        Fail("Multiple update statements for iteration variable %s. Only one update statement is allowed.", itervar)
      }

      s.expression match {
        case e: OperatorExpression => e.operator match {
          case Plus if e.first.equals(itervar) =>
            e.second match {
              case c: ConstantExpression =>
                updateStmnt = Some(Plus, e.second)
              case o: OperatorExpression if o.operator == UMinus && o.first.isInstanceOf[ConstantExpression] =>
                updateStmnt = Some(Minus, o.first)
            }
          case Plus if e.second.equals(itervar) =>
            e.first match {
              case c: ConstantExpression =>
                updateStmnt = Some(Plus, e.first)
              case o: OperatorExpression if o.operator == UMinus && o.first.isInstanceOf[ConstantExpression] =>
                updateStmnt = Some(Minus, o.first)
            }
          case Minus if e.first.equals(itervar) =>
            updateStmnt = Some(e.operator, e.second)
          case Minus if e.second.equals(itervar) =>
            updateStmnt = Some(e.operator, e.first)
          case FloorDiv if e.first.equals(itervar) =>
            updateStmnt = Some(e.operator, e.second)
          //        case (FloorDiv) if e.second.equals(itervar) =>
          //TODO OS should there be a warning here
          //          updateOp = e.operator
          //          updateConstant = e.first
          case Mult if e.first.equals(itervar) =>
            updateStmnt = Some(e.operator, e.second)
          case Mult if e.second.equals(itervar) =>
            updateStmnt = Some(e.operator, e.first)
          case _ => Warning("%s is not supported in the update statement at %s.", e.operator, e.getOrigin)
        }
        case _ =>
      }

      super.visit(s)
      // C is a ConstantExpression
      // -C is a OperatorExpression UMinus C
      //TODO OS treat -C + i as i - C

    }


  }

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
        case LT => e.first.equals(itervar)
        case LTE => e.first.equals(itervar)
        case GT => e.second.equals(itervar)
        case GTE => e.second.equals(itervar)
        case EQ => e.first.equals(itervar) ^ e.second.equals(itervar)
        case NEQ => e.first.equals(itervar) ^ e.second.equals(itervar)

        //TODO OS which cases do we need
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

  def findBounds(s: LoopStatement, K: ConstantExpression, itervar: ASTNode, updateStmnt: (StandardOperator, ASTNode)): Option[(ASTNode, ASTNode, Contract)] = {
    val cb = new ContractBuilder
    val invs = s.getContract.invariant
    var lowerbounds: Set[ASTNode] = Set.empty
    var upperbounds: Set[ASTNode] = Set.empty

    ASTUtils.conjuncts(invs, Star, And).forEach {
      case e: OperatorExpression => e.operator match {
        case LT if e.second.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          lowerbounds ++= Set(create.expression(Plus, e.first, create.constant(1))) // Lowerbound
        case LTE if e.second.equals(itervar) =>
          if (updateStmnt._1 == Plus) {
            val updatedLowerbound = create.expression(Plus, e.first, create.expression(Mult, rewrite(updateStmnt._2), rewrite(K)))
            val updatedInvariant = create.expression(LTE, updatedLowerbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else {
            //TODO OS this case is for debug purposes
            cb.appendInvariant(rewrite(e))
          }

          lowerbounds ++= Set(e.first) // Lowerbound
        case GT if e.second.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          lowerbounds ++= Set(create.expression(Plus, e.first, create.constant(1))) // Lowerbound
        case GTE if e.second.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          lowerbounds ++= Set(e.first) // Lowerbound
        case LT if e.first.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          upperbounds ++= Set(create.expression(Minus, e.second, create.constant(1))) // Upperbound
        case LTE if e.first.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          upperbounds ++= Set(e.second) // Upperbound
        case GT if e.first.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          upperbounds ++= Set(create.expression(Minus, e.second, create.constant(1))) // Upperbound
        case GTE if e.first.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
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
    if (s.getUnroll == null || s.getUnroll.name != GPUOptName.LoopUnroll) {
      super.visit(s)
      return
    } else if (inLoop) {
      Warning("Only one loop can be optimized at a time. By default, the outer loop is optimized first. Please run VerCors again with the output to optimize the inner loops %s", s.getOrigin)
      super.visit(s)
      return
    }
    inLoop = true

    // The iteration variable, a NameExpression
    val itervar = s.getUnroll.args.head
    // The number to unroll, an integer constant
    val K: ConstantExpression = s.getUnroll.args(1).asInstanceOf[ConstantExpression]
    if (K.value.asInstanceOf[IntegerValue].value <= 0) {
      Warning("Loop unroll optimization is not performed at %s", s.getUnroll.getOrigin)
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
//    Progress("Update Statement: %s in", updateStmt)

    //////////////////////////////////////
    /// find the lower and upperbounds ///
    //////////////////////////////////////
    val maybeBounds = findBounds(s, K, itervar, updateStmt)
    if (maybeUpdateStmt.isEmpty) {

    }
    val (a, b, contract) = maybeBounds.get

//    Progress("Lowerbound = %s", a)
//    Progress("Upperbound = %s", b)

    /** TODO OS check if you can actually unroll
     * 1. generate method for checking unroll
     * 2. Send to Viper
     * 3. Process output
     */


    ////////////////////////////////
    /// check if unroll is possible ///
    ////////////////////////////////
    //TODO OS how to get a free name
    //TODO OS can all the methods be static or not?
    val methodsStatic = true
    val nameOfU = "U" + (Math.random() * 100).asInstanceOf[Int]
    val nameOfInc = "inc" + (Math.random() * 100).asInstanceOf[Int]


    val mapOfargsU = new util.LinkedHashMap[String, Type](NameScanner.freeVars(s.getEntryGuard))
    val argsOfU = genPars(mapOfargsU)

    val argsCallU = argsOfU.map(_.name).map(
      k => if (itervar.isName(k)) invoke(null, nameOfInc, rewrite(itervar)) else create.local_name(k)
    ).toSeq

    val invokeU = invoke(null, nameOfU, argsCallU: _*)

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




    //    pure inline int inc(int i) = i + 1;

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

    //TODO OS invokeU should not have inc(i), it should have i
    bodyOfCheck.add(create special(ASTSpecial.Kind.Assert, gt(size(invokeU), K)))

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


    Progress("U: \n%s", methodU)
    Progress("inc: \n%s", methodInc)
    Progress("Check method: \n%s", methodCheck)



//    val checkProgram = new ProgramUnit()
//    checkProgram.add(methodInc)
//    checkProgram.add(methodU)
//    checkProgram.add(methodCheck)
//
//    val inputReport = new PassReport(new ProgramUnit)
//    inputReport.setOutput(checkProgram)
//    inputReport.add(new ErrorDisplayVisitor)
//
//    new AbstractTypeCheck(inputReport, checkProgram).check()
//    inputReport.setOutput(new SilverClassReduction(inputReport.getOutput).rewriteAll)
//    new AbstractTypeCheck(inputReport, checkProgram).check()
//
//    val report = vct.silver.SilverBackend.TestSilicon(inputReport, "silicon")
//
//    Progress("So, what is the verdict?")
//
//


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

    1 until K.value.asInstanceOf[IntegerValue].value foreach {
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
