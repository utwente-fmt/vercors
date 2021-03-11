package vct.col.rewrite.gpgpuoptimizations

import hre.lang.System.{Output, Progress}
import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator.{And, EQ, FloorDiv, GT, GTE, LT, LTE, Minus, Mult, NEQ, Plus, Star}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{Contract, GPUOptName, ProgramUnit}
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}

import scala.collection.JavaConverters._

//TODO OS Have a warning when there is an optimization but VerCors is ran normally.
case class IterationMerging(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private var inLoop: Boolean = false

  override def visit(s: LoopStatement): Unit = {
    if (s.getGpuopt == null || s.getGpuopt.name != GPUOptName.IterationMerging) {
      super.visit(s)
      return
    } else if (inLoop) {
      //TODO OS think of a better warning message
      Warning("Only one loop can be optimized at a time. By default, the outer loop is optimized first. Please run VerCors again with the output to optimize the inner loops %s", s.getOrigin)
      super.visit(s)
      return
    }
    inLoop = true

    val itervar = s.getGpuopt.args.head
    val M = s.getGpuopt.args(1)
      .asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value
    if (M <= 1) {
      Warning("Iteration merging optimization is not performed at %s", s.getGpuopt.getOrigin)
      return
    }

    val maybeUpdateStmt = findUpdateStatement(s, itervar)
    if (maybeUpdateStmt.isEmpty) {
      Fail("The iteration variable %s is never updated.", itervar)
    }

    //TODO OS, after making findBounds more generic, remove create.constant(1)
    val maybeBounds = findBounds(s, create.constant(1), itervar, maybeUpdateStmt.get)
    if (maybeBounds.isEmpty) {
      Fail("Could not find bounds for %s at ", itervar, s.getOrigin)
    }

    val op = maybeUpdateStmt.get._1
    //TODO OS, change this after changing the grammar
    val C = maybeUpdateStmt.get._2
      .asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value

    val a = maybeBounds.get._1.asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value
    val b = maybeBounds.get._2.asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value


    val (start, condOp, update) = op match {
      case Mult => (a, (lhs: Int) => lhs < b, (lhs: Int) => lhs * C)
      case Plus => (a, (lhs: Int) => lhs < b, (lhs: Int) => lhs + C)
      case Minus => (b, (lhs: Int) => lhs > a, (lhs: Int) => lhs - C)
      case FloorDiv => (b, (lhs: Int) => lhs > a, (lhs: Int) => lhs / C)
      case _ =>
        Fail("unsupported operator")
        return
    }

    val I = calculateIterations(
      start,
      condOp,
      update
    )

    val K = I % M

    if (M > I) {
      Fail("The number of iterations is smaller than the number of iterations to merge at %s", s.getGpuopt.getOrigin)
    }

    var loopToMerge = s
    var xBodies = 0
    if (K != 0) {
      val tmp = s.getGpuopt
      s.setGpuopt(create.gpuoptimization(GPUOptName.LoopUnroll, Seq(itervar, constant(K)).asJava))

      val unrollLoopPass = LoopUnroll(null, false)
      loopToMerge = unrollLoopPass.rewrite(s)
      val unrolledBodies = unrollLoopPass.tmpBlock
      s.setGpuopt(tmp)

      unrolledBodies.forEachStmt(st => current_sequence().add(copy_rw.rewrite(st)))
    }

    val originalBody = copy_rw.rewrite(s.getBody)
    val bodyWithoutDecls = create.block()
    val bodyWithoutDeclsWithoutUpdate = create.block()
    val newBody = create.block()
    val removeDecl = new ReplaceDeclarationsByAssignments(null)

    s.getBody match {
      case b: BlockStatement =>
        b.getStatements.foreach(st => bodyWithoutDecls.add(removeDecl.rewrite(st)))
        b.getStatements.foreach(st => bodyWithoutDeclsWithoutUpdate.add(removeDecl.rewrite(st)))
      case _ =>
        bodyWithoutDecls.add(removeDecl.rewrite(s.getBody))
    }
    if (s.getUpdateBlock != null)
      s.getUpdateBlock match {
        case b: BlockStatement =>
          b.forEachStmt(st => bodyWithoutDecls.add(removeDecl.rewrite(st)))
        case notablock =>
          bodyWithoutDeclsWithoutUpdate.add(removeDecl.rewrite(notablock))
      }


    if (K == 0) {
      originalBody match {
        case b: BlockStatement =>
          b.getStatements.foreach(stmt => newBody.add(copy_rw.rewrite(stmt)))
        case _ =>
          newBody.add(copy_rw.rewrite(s.getBody))
      }
      if (s.getUpdateBlock != null)
        s.getUpdateBlock match {
          case b: BlockStatement =>
            b.getStatements.foreach(stmt => newBody.add(copy_rw.rewrite(stmt)))
          case notablock =>
            newBody.add(copy_rw.rewrite(notablock))
        }
      xBodies = 1
    }

    xBodies until M foreach {
      i =>
        if (i == M - 1)
          bodyWithoutDeclsWithoutUpdate.forEachStmt(stmt => newBody.add(copy_rw.rewrite(stmt)))
        else
          bodyWithoutDecls.forEachStmt(stmt => newBody.add(copy_rw.rewrite(stmt)))
    }





    //TODO OS do we have to rewrite here?
    val newContract = copy_rw.rewrite(loopToMerge.getContract)
    val cb = new ContractBuilder
    rewrite(loopToMerge.getContract, cb);
    cb.prependInvariant(
      eq(
        create.expression(StandardOperator.Mod, rewrite(itervar), constant(M)),
        constant(K)
      )
    )

    result = create.loop(
      rewrite(loopToMerge.getInitBlock),
      rewrite(loopToMerge.getEntryGuard),
      rewrite(loopToMerge.getExitGuard),
      rewrite(loopToMerge.getUpdateBlock),
      newBody,
      cb.getContract()
    )

    inLoop = false
  }


  // op is a function which takes as a first argument the value of i
  def calculateIterations(start: Int, condOp: ((Int) => Boolean),
                          op: Int => Int): Int = {
    var a = start
    var Is = Seq(a)
    while (condOp(a)) {

      a = op(a) // the constant C is already implicit
      Is ++= Seq(a)
    }

    Is.length - 1
  }


  /////////////////////
  /// TO BE REMOVED ///
  /////////////////////
  /// TO BE REMOVED ///
  /////////////////////
  /// TO BE REMOVED ///
  /////////////////////
  /// TO BE REMOVED ///
  /////////////////////
  /// TO BE REMOVED ///
  /////////////////////

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

  //TODO OS, remove K from the arguments.
  def findBounds(s: LoopStatement, K: ConstantExpression, itervar: ASTNode, updateStmnt: (StandardOperator, ASTNode)): Option[(ASTNode, ASTNode)] = {
    var lowerbounds: Set[ASTNode] = Set.empty
    var upperbounds: Set[ASTNode] = Set.empty

    ASTUtils.conjuncts(s.getContract.invariant, Star, And).forEach {
      case e: OperatorExpression => e.operator match {
        case LT if e.second.equals(itervar) =>
          lowerbounds ++= Set(create.expression(Plus, e.first, create.constant(1))) // Lowerbound
        case LTE if e.second.equals(itervar) =>
          lowerbounds ++= Set(e.first) // Lowerbound
        case GT if e.second.equals(itervar) =>
          lowerbounds ++= Set(create.expression(Plus, e.first, create.constant(1))) // Lowerbound
        case GTE if e.first.equals(itervar) =>
          lowerbounds ++= Set(e.second) // Lowerbound
        case GTE if e.second.equals(itervar) =>
          upperbounds ++= Set(e.first) // Upperbound
        case LT if e.first.equals(itervar) =>
          upperbounds ++= Set(create.expression(Minus, e.second, create.constant(1))) // Upperbound
        case LTE if e.first.equals(itervar) =>
          upperbounds ++= Set(e.second) // Upperbound
        case GT if e.first.equals(itervar) =>
          upperbounds ++= Set(create.expression(Minus, e.second, create.constant(1))) // Upperbound

        case _ =>
      }
      case default =>
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

    Option((a, b))
  }

}
