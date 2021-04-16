package vct.col.rewrite.gpgpuoptimizations

import hre.lang.System.{Fail, Output, Progress}
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort}
import vct.col.ast.expr.{NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator.{And, EQ, FloorDiv, GT, GTE, LT, LTE, Minus, Mult, NEQ, Plus, Star}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{Contract, DeclarationStatement, IterationMerging, ProgramUnit}
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}
import vct.col.rewrite.gpgpuoptimizations.LoopOperations.findBounds

import scala.collection.JavaConverters._

//TODO OS Have a warning when there is an optimization but VerCors is ran normally.
case class MergeIterations(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private var inLoop: Boolean = false
  private val powFuncName = "vct_mi_pow"
  private val divFuncName = "vct_mi_div"

  override def visit(s: LoopStatement): Unit = {
    if (s.getGpuopt == null || !s.getGpuopt.isInstanceOf[IterationMerging]) {
      super.visit(s)
      return
    } else if (inLoop) {
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
      Fail("Iteration merging optimization is not performed for zero or one at %s", s.getGpuopt.getOrigin)
      return
    }

    val updateStmt = LoopOperations.findUpdateStatement(s, itervar)

    val bounds = findBounds(s, itervar, create)

    val op = updateStmt._1
    val C = updateStmt._2
      .asInstanceOf[ConstantExpression].value
      .asInstanceOf[IntegerValue].value

    if (!bounds._1.isInstanceOf[ConstantExpression] &&
      !(bounds._1.isInstanceOf[OperatorExpression] &&
        bounds._1.asInstanceOf[OperatorExpression].operator == StandardOperator.UMinus &&
        bounds._1.asInstanceOf[OperatorExpression].first.isInstanceOf[ConstantExpression])
    ) {
      Fail("The lowerbound of %s is not a constant, but %s", itervar, bounds._1)
    } else if (!bounds._2.isInstanceOf[ConstantExpression] &&
      !(bounds._2.isInstanceOf[OperatorExpression] &&
        bounds._2.asInstanceOf[OperatorExpression].operator == StandardOperator.UMinus &&
        bounds._2.asInstanceOf[OperatorExpression].first.isInstanceOf[ConstantExpression])
    ) {
      Fail("The upperbound of %s is not a constant, but %s", itervar, bounds._2)
    }
    val a = bounds._1 match {
      case const: ConstantExpression => const.value.asInstanceOf[IntegerValue].value + bounds._3
      case _ => -1* bounds._1.asInstanceOf[OperatorExpression].first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value  + bounds._3
    }
    val b = bounds._2 match {
      case const: ConstantExpression => const.value.asInstanceOf[IntegerValue].value + bounds._4
      case _ => -1* bounds._2.asInstanceOf[OperatorExpression].first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value + bounds._4
    }

    val (start, cond, update) = op match {
      case Mult =>      (a, getCondition(s, itervar), (lhs: Int) => lhs * C)
      case Plus =>      (a, getCondition(s, itervar), (lhs: Int) => lhs + C)
      case Minus =>     (b, getCondition(s, itervar), (lhs: Int) => lhs - C)
      case FloorDiv =>  (b, getCondition(s, itervar), (lhs: Int) => lhs / C)
      case _ => Fail("unsupported operator")
        return
    }

    val I = calculateIterations(start, cond, update)

    val K = I % M

    if (M > I) {
      Fail("The number of iterations is less than the number of iterations to merge at %s", s.getGpuopt.getOrigin)
    }
    // This variable newL is possibly added to the list of declarations before the optimization happens
    val newL = "vct_mi_l";

    var loopToMerge = s
    var xBodies = 0
    if (K != 0) {
      val tmp = copy_rw.rewrite(s.getGpuopt)
      s.setGpuopt(create.opt_loop_unroll(itervar, constant(K)))

      val unrollLoopPass = UnrollLoops(null, false)
      val copyOfLoop = copy_rw.rewrite(s)

      var newLoopBody = copy_rw.rewrite(s.getBody)
      val updateLStmnt = create.assignment(name(newL), plus(name(newL), constant(1)))
      newLoopBody = newLoopBody match {
        case block: BlockStatement => block.add(updateLStmnt)
        case nab => {
          create.block().add(nab).add(updateLStmnt)
        }
      }
      var loopWithUnrolls = create.block(ASTUtils.replace(s.getBody, newLoopBody, s))


      loopWithUnrolls = unrollLoopPass.rewrite(loopWithUnrolls)
      loopToMerge = loopWithUnrolls.getStatement(loopWithUnrolls.getLength-1).asInstanceOf[LoopStatement]
      val unrolledBodies = loopWithUnrolls.getStatements.take(loopWithUnrolls.getLength-1)
      s.setGpuopt(tmp)
      current_sequence().add(create.field_decl(newL, create.primitive_type(PrimitiveSort.Integer), constant(0)))

      unrolledBodies.foreach(st => current_sequence().add(copy_rw.rewrite(st)))
    }

    val originalBody = copy_rw.rewrite(s.getBody)
    val bodyWithoutDecls = create.block()
    val bodyWithoutDeclsWithoutUpdate = create.block()
    val newBody = create.block()
    val removeDecl = new ReplaceDeclarationsByAssignments(null)
    val removeDecl2 = new ReplaceDeclarationsByAssignments(null)

    s.getBody match {
      case b: BlockStatement =>
        b.getStatements.foreach(st => bodyWithoutDecls.add(removeDecl.rewrite(st)))
        b.getStatements.foreach(st => bodyWithoutDeclsWithoutUpdate.add(removeDecl2.rewrite(st)))
      case _ =>
        bodyWithoutDecls.add(removeDecl.rewrite(s.getBody))
        bodyWithoutDeclsWithoutUpdate.add(removeDecl.rewrite(s.getUpdateBlock))
    }
    s.getUpdateBlock match {
      case null =>
      case b: BlockStatement =>
        b.forEachStmt(st => bodyWithoutDecls.add(removeDecl.rewrite(st)))
      case notablock =>
        bodyWithoutDecls.add(removeDecl.rewrite(notablock))
    }

    bodyWithoutDecls.add(create.assignment(name(newL), plus(name(newL), constant(1))))
    bodyWithoutDeclsWithoutUpdate.add(create.assignment(name(newL), plus(name(newL), constant(1))))

    //////////////////////////////////////
    //////////////////////////////////////
    //////////////////////////////////////
    val cb = new ContractBuilder
    rewrite(loopToMerge.getContract, cb)

    op match {
      case Plus => {
        //  i % M == ((a+c*I%M)) % M
        val newConst = a + C*K
        cb.prependInvariant(
          eq(
            create.expression(StandardOperator.Mod, rewrite(itervar), constant(M)),
            create.expression(StandardOperator.Mod, constant(newConst), constant(M))
          )
        )
//        i==a+c*l; plus
        cb.prependInvariant(eq(
          rewrite(itervar),
          plus(constant(a), mult(constant(C), name(newL)))
        ))
      }
      case Minus => {
        //i % M == ((b-c*I%M)) % M
        val newConst = b - C*K
        cb.prependInvariant(
          eq(
            create.expression(StandardOperator.Mod, rewrite(itervar), constant(M)),
            create.expression(StandardOperator.Mod, constant(newConst), constant(M))
          )
        )
//        i==b-c*l; minus
        cb.prependInvariant(eq(
          rewrite(itervar),
          minus(constant(b), mult(constant(C), name(newL)))
        ))
      }
      case Mult => {
        //Add a new variable before the loop.
//        loop_invariant k % m == (I % m);
//        loop_invariant I % m <= k;
//        loop_invariant i == a * pow(c,k);
          addPowFunc()

          cb.prependInvariant(eq(
            create.expression(StandardOperator.Mod, name(newL), constant(M)),
            constant(K),
          ))
          cb.prependInvariant(eq(
            rewrite(itervar),
            mult(constant(a), invoke(null, powFuncName, constant(C), name(newL)))
          ))
      }
      case FloorDiv => {
        //        0 <= k
        //        k <= I+1
        //        k % M == K
        //        i == b/(C^k)

        addDivFunc()

        cb.prependInvariant(eq(
          rewrite(itervar),
          invoke(null, divFuncName, constant(b), constant(C), name(newL)))
        )

        cb.prependInvariant(eq(
          create.expression(StandardOperator.Mod, name(newL), constant(M)),
          constant(K),
        ))

      }
      case _ => Fail("unsupported operator")
    }
    cb.prependInvariant(lte(name(newL), constant(I+1)))
    cb.prependInvariant(lte(constant(K),name(newL)))

    //////////////////////////////////////
    //////////////////////////////////////
    //////////////////////////////////////
    if (K == 0) {
      current_sequence().add(create.field_decl(newL, create.primitive_type(PrimitiveSort.Integer), constant(0)))
      removeDecl.declaredVars.foreach({decl => newBody.add(rewrite(decl))})

      bodyWithoutDecls match {
        case b: BlockStatement =>
          b.getStatements.foreach(stmt => newBody.add(copy_rw.rewrite(stmt)))
        case _ =>
          newBody.add(copy_rw.rewrite(s.getBody))
      }
//      if (s.getUpdateBlock != null)
//        s.getUpdateBlock match {
//          case b: BlockStatement =>
//            b.getStatements.foreach(stmt => newBody.add(copy_rw.rewrite(stmt)))
//          case notablock =>
//            newBody.add(copy_rw.rewrite(notablock))
//        }
      xBodies = 1
    }

    xBodies until M foreach {
      i =>
        if (i == M - 1)
          bodyWithoutDeclsWithoutUpdate.forEachStmt(stmt => newBody.add(copy_rw.rewrite(stmt)))
        else
          bodyWithoutDecls.forEachStmt(stmt => newBody.add(copy_rw.rewrite(stmt)))
    }

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

    val Is = getIterValues(start, condOp, op)
    Is.length - 1
  }

  def getIterValues(start: Int, condOp: ((Int) => Boolean),
                          op: Int => Int): Seq[Int] = {
    var a = start
    var Is = Seq(a)
    while (condOp(a)) {

      a = op(a) // the constant C is already implicit
      Is ++= Seq(a)
    }
    Is
  }


  def addPowFunc(): Unit = {
    if (currentTargetClass.find(powFuncName, null, null) == null) {
//      requires y >= 0;
//      ensures (y == 0) ==> \result == 1;
//      ensures (y != 0) ==> \result == x * pow(x, y-1);
//      pure static int pow(int x, int y) = (y == 0) ? 1 : x * pow(x, y-1);
      val cb = new ContractBuilder()
      val x = "x"
      val y = "y"
      cb.requires(gte(name(y), constant(0)))
      cb.ensures(implies(
        eq(name(y), constant(0)),
        eq(create.reserved_name(ASTReserved.Result), constant(1))
      ))
      cb.ensures(implies(
        neq(name(y), constant(0)),
        eq(
          create.reserved_name(ASTReserved.Result),
          mult(
            name(x),
            invoke(null, powFuncName, name(x), minus(name(y), constant(1)))
          )
        )
      ))

      val powfunc = create.function_decl(
        create.primitive_type(PrimitiveSort.Integer),
        cb.getContract(false),
        powFuncName,
        Seq(
          new DeclarationStatement("x", create.primitive_type(PrimitiveSort.Integer)),
          new DeclarationStatement("y", create.primitive_type(PrimitiveSort.Integer)),
        ).asJava,
        ite(
          eq(name(y), constant(0)),
          constant(1),
          mult(
            name(x),
            invoke(null, powFuncName, name(x), minus(name(y), constant(1)))
          )
        )
      )
      currentTargetClass.add_static(powfunc)
    }
  }


  def getCondition(s: LoopStatement, itervar: ASTNode): ((Int) => Boolean) = {
    val cond = s.getEntryGuard
    val condCheck: ((Int) => Boolean) = cond match {
      case e: OperatorExpression => e.operator match {
        case LT if e.first.equals(itervar) &&
          e.second.isInstanceOf[ConstantExpression] &&
          e.second.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue] =>
          ((lhs: Int) => lhs < e.second.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value)
        case LT if e.second.equals(itervar) &&
          e.first.isInstanceOf[ConstantExpression] &&
          e.first.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue] =>
          (lhs: Int) => e.first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value < lhs



        case LTE if e.first.equals(itervar) &&
          e.second.isInstanceOf[ConstantExpression] &&
          e.second.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]  =>
          (lhs: Int) => lhs <= e.second.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value

        case LTE if e.second.equals(itervar) &&
          e.first.isInstanceOf[ConstantExpression] &&
          e.first.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]  =>
          (lhs: Int) => e.first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value <= lhs

        case GT if e.first.equals(itervar) &&
          e.second.isInstanceOf[ConstantExpression] &&
          e.second.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]  =>
          (lhs: Int) => lhs > e.second.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value

        case GT if e.second.equals(itervar) &&
          e.first.isInstanceOf[ConstantExpression] &&
          e.first.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]  =>
          (lhs: Int) => e.first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value > lhs

        case GTE if e.first.equals(itervar) &&
          e.second.isInstanceOf[ConstantExpression] &&
          e.second.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]  =>
          (lhs: Int) => lhs >= e.second.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value

        case GTE if e.second.equals(itervar) &&
          e.first.isInstanceOf[ConstantExpression] &&
          e.first.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]  =>
          (lhs: Int) => e.first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value >= lhs


        //        case LT => e.first.equals(itervar) ^ e.second.equals(itervar)
        //        case LTE => e.first.equals(itervar) ^ e.second.equals(itervar)
        //        case GT => e.first.equals(itervar) ^ e.second.equals(itervar)
        //        case GTE => e.first.equals(itervar) ^ e.second.equals(itervar)
        //        case EQ => e.first.equals(itervar) ^ e.second.equals(itervar)
        //        case NEQ => e.first.equals(itervar) ^ e.second.equals(itervar)
        case _ =>
          Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
          null
      }
      case _ =>
        Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
        null // Needed because Fail is not a Nothing.
    }
    condCheck
  }



  def addDivFunc(): Unit = {
    if (currentTargetClass.find(divFuncName, null, null) == null) {
      //  requires k >= 0;
      //  requires y != 0;
      //  ensures (k == 0) ==> \result == x;
      //  ensures (k != 0) ==> \result == div(x, y, k-1)/y;
      //  pure static int div(int x, int y, int k) = (k == 0) ? x : div(x, y, k-1)/y;

      val cb = new ContractBuilder()
      val x = "x"
      val y = "y"
      val k = "k"
      cb.requires(gte(name(k), constant(0)))
      cb.requires(neq(name(y), constant(0)))
      cb.ensures(implies(
        eq(name(k), constant(0)),
        eq(create.reserved_name(ASTReserved.Result), name(x))
      ))
      cb.ensures(implies(
        neq(name(k), constant(0)),
        eq(
          create.reserved_name(ASTReserved.Result),
          floordiv(
            invoke(null, divFuncName, name(x), name(y), minus(name(k), constant(1))),
            name(y)
          )
        )
      ))

      val divFunc = create.function_decl(
        create.primitive_type(PrimitiveSort.Integer),
        cb.getContract(false),
        divFuncName,
        Seq(
          new DeclarationStatement("x", create.primitive_type(PrimitiveSort.Integer)),
          new DeclarationStatement("y", create.primitive_type(PrimitiveSort.Integer)),
          new DeclarationStatement("k", create.primitive_type(PrimitiveSort.Integer)),
        ).asJava,
        ite(
          eq(name(k), constant(0)),
          name(x),
          floordiv(
            invoke(null, divFuncName, name(x), name(y), minus(name(k), constant(1))),
            name(y)
          )
        )
      )
      currentTargetClass.add_static(divFunc)
    }
  }

}
