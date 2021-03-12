package vct.col.rewrite.gpgpuoptimizations

import hre.lang.System.{Output, Progress}
import vct.col.ast.expr.{NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator.{And, EQ, FloorDiv, GT, GTE, LT, LTE, Minus, Mult, NEQ, Plus, Star}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{Contract, IterationMerging, ProgramUnit}
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}
import vct.col.rewrite.gpgpuoptimizations.LoopOperations.findBounds

import scala.collection.JavaConverters._

//TODO OS Have a warning when there is an optimization but VerCors is ran normally.
case class MergeIterations(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private var inLoop: Boolean = false

  override def visit(s: LoopStatement): Unit = {
    if (s.getGpuopt == null || s.getGpuopt.isInstanceOf[IterationMerging]) {
      super.visit(s)
      return
    } else if (inLoop) {
      //TODO OS think of a better warning message
      Warning("Only one loop can be optimized at a time. By default, the outer loop is optimized first. Please run VerCors again with the output to optimize the inner loops %s", s.getOrigin)
      super.visit(s)
      return
    }
    inLoop = true

    val itervar = s.getGpuopt.args.head.asInstanceOf[NameExpression]
    val M = s.getGpuopt.args(1)
      .asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value
    if (M <= 1) {
      Warning("Iteration merging optimization is not performed at %s", s.getGpuopt.getOrigin)
      return
    }

    val updateStmt = LoopOperations.findUpdateStatement(s, itervar)

    val bounds = findBounds(s, itervar, create)

    val op = updateStmt._1
    val C = updateStmt._2
      .asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value

    val a = bounds._1.asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value
    val b = bounds._2.asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value

    val (start, condOp, update) = op match {
      case Mult =>      (a, (lhs: Int) => lhs < b, (lhs: Int) => lhs * C)
      case Plus =>      (a, (lhs: Int) => lhs < b, (lhs: Int) => lhs + C)
      case Minus =>     (b, (lhs: Int) => lhs > a, (lhs: Int) => lhs - C)
      case FloorDiv =>  (b, (lhs: Int) => lhs > a, (lhs: Int) => lhs / C)
      case _ => Fail("unsupported operator")
        return
    }

    val I = calculateIterations(start, condOp, update)

    val K = I % M

    if (M > I) {
      Fail("The number of iterations is smaller than the number of iterations to merge at %s", s.getGpuopt.getOrigin)
    }

    var loopToMerge = s
    var xBodies = 0
    if (K != 0) {
      val tmp = s.getGpuopt
      s.setGpuopt(create.opt_loop_unroll(itervar, constant(K)))

      val unrollLoopPass = UnrollLoops(null, false)
      var loopWithUnrolls = create.block(copy_rw.rewrite(s))
      loopWithUnrolls = unrollLoopPass.rewrite(loopWithUnrolls)
      loopToMerge = loopWithUnrolls.getStatement(loopWithUnrolls.getLength-1).asInstanceOf[LoopStatement]
      val unrolledBodies = loopWithUnrolls.getStatements.take(loopWithUnrolls.getLength-1)
      s.setGpuopt(tmp)

      unrolledBodies.foreach(st => current_sequence().add(copy_rw.rewrite(st)))
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

}
