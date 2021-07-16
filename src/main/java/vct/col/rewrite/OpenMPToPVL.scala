package vct.col.rewrite

import vct.col.ast.`type`.Type
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.expr.{NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.langspecific.c._
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelBlock}
import vct.col.ast.stmt.decl.{ASTSpecial, Contract, DeclarationStatement, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.AbstractRewriter
import vct.col.util.FeatureScanner

class OpenMPToPVL(source: ProgramUnit) extends AbstractRewriter(source) {
  def simdLoopToPPL(loop: OMPLoop): PPLBlock = {
    val lenValue = getSimdLen(loop.options) match {
      case Some(len) => len
      case None => throw Failure("simdlen required for 'omp for simd' loop")
    }

    val (name, t, start, end) = getParDeclFromLoop(loop.loop) match {
      case Some(vals) => vals
      case None => throw Failure("For loop not representable as parallel block")
    }

    val len = create constant (lenValue)

    /* inner is a vector block of `len` threads
       outer is a PPLParallel of `(end - start) / len` threads */

    val outerStart = create expression(StandardOperator.FloorDiv, start, len)
    val outerEnd = create expression(StandardOperator.FloorDiv, end, len)
    val outerName = "par_" + name
    val outer = create local_name outerName
    val outerDecl = create field_decl(outerName, t, create expression(StandardOperator.RangeSeq, outerStart, outerEnd))
    val outerInc = create expression(StandardOperator.Plus, outer, create constant 1)

    val innerDecl = create field_decl(name, t, create expression(StandardOperator.RangeSeq,
      create expression(StandardOperator.Mult, outer, len),
      create expression(StandardOperator.Mult, outerInc, len)
    ))
    val inner = create vector_block(innerDecl, rewrite(loop.loop.getBody).asInstanceOf[BlockStatement])

    val outerBody = create.block()
    outerBody.add(create special(ASTSpecial.Kind.Assume,
      create.expression(StandardOperator.LTE,
        create expression(StandardOperator.Mult, outerInc, len),
        create expression(StandardOperator.Mult, outerEnd, len))
    ))
    outerBody.add(inner)

    PPLBlock(Seq(outerDecl), outerBody, rewrite(loop.loop.getContract), loop)
  }

  def translate(block: BlockStatement): Seq[PPL] = block.getStatements.toIndexedSeq.map(matchNode)

  def matchNode(node: ASTNode): PPL = node match {
    case loop@OMPFor(_, _) => forLoopToPPL(loop)
    case loop@OMPParallelFor(_, _) => forLoopToPPL(loop)
    case loop@OMPForSimd(_, _) => simdLoopToPPL(loop)
    case OMPSections(block) => block.getStatements.map {
      case OMPSection(block) => compose(translate(block))
      case _ => throw Failure("omp sections block may only contain omp section blocks")
    }.reduce(PPLPar)
    case OMPParallel(block, options, contract) => compose(translate(block))
    case _ => throw Failure("??")
  }

  def compose(xs: Seq[PPL]): PPL = {
    val fused = bundle[PPL](PPLFuse, (x, y) => x.isFor && y.isFor && x.hasStaticSchedule && y.hasStaticSchedule && x.isNoWait, xs)
    val pared = bundle[PPL](PPLPar, (x, _) => x.isNoWait, fused)
    pared.reduce(PPLSeq)
  }

  def bundle[T](op: (T, T) => T, cond: (T, T) => Boolean, xs: Seq[T]): Seq[T] = {
    xs.init.foldRight(Seq(xs.last))((x, r) => {
      if (cond(x, r.head)) {
        op(x, r.head) +: r.tail
      } else {
        x +: r
      }
    })
  }

  override def visit(par: OMPParallel): Unit = {
    result = matchNode(par).toCOL(par.contract)
  }

  override def visit(par: OMPParallelFor): Unit = {
    result = create region(null, forLoopToPPL(par).toParBlock(null, Array()))
  }

  def forLoopToPPL(loop: OMPLoop): PPLBlock = {
    val parDecl: DeclarationStatement = getParDeclFromLoop(loop.loop) match {
      case Some((name, t, start, end)) =>
        create field_decl(name, t,
          create expression(StandardOperator.RangeSeq, start, end))
      case None => throw Failure("For loop not representable as parallel block")
    }

    PPLBlock(Seq(parDecl), rewrite(loop.loop.getBody).asInstanceOf[BlockStatement], rewrite(loop.loop.getContract), loop)
  }

  /**
   * Checks whether a loop can be transformed to a par block, and returns an appropriate declaration for the par if so
   *
   * @param loop The loop to check
   * @return The variable the loop originally declared and its range
   */
  private def getParDeclFromLoop(loop: LoopStatement): Option[(String, Type, ASTNode, ASTNode)] = {
    // We assign exactly one variable
    val initStatement = loop.getInitBlock match {
      case block: BlockStatement if block.size == 1 => block.get(0)
      case other => other
    }
    val (start, name, t) = initStatement match {
      case decl: DeclarationStatement => decl.init match {
        case None => return None
        case Some(start) =>
          (start, decl.name, decl.`type`)
      }
      case op: OperatorExpression if op.operator == StandardOperator.Assign => op.arg(0) match {
        case name: NameExpression =>
          (op.arg(1), name.getName, name.getType)
        case _ => return None
      }
      case _ => return None
    }

    // The entry guard is of the form var < expr
    val end = loop.getEntryGuard match {
      case op: OperatorExpression if op.operator == StandardOperator.LT =>
        if (!op.arg(0).isName(name)) {
          return None
        }
        op.arg(1)
      case _ => return None
    }

    // The update only increments our one variable
    val updateStatement = loop.getUpdateBlock match {
      case block: BlockStatement if block.size == 1 => block.get(0)
      case other => other
    }

    val incremented: NameExpression = updateStatement match {
      case OperatorExpression(StandardOperator.PostIncr, List(incrName: NameExpression)) => incrName
      case OperatorExpression(StandardOperator.PreIncr, List(incrName: NameExpression)) => incrName
      case OperatorExpression(StandardOperator.AddAssign,
      List(incrName: NameExpression, ConstantExpression(IntegerValue(1)))
      ) => incrName
      case OperatorExpression(StandardOperator.Assign, List(
      incrName: NameExpression,
      OperatorExpression(StandardOperator.Plus, List(otherName: NameExpression, ConstantExpression(IntegerValue(1))))
      )) if incrName == otherName => incrName
      case assign: AssignmentStatement => assign.expression match {
        case OperatorExpression(StandardOperator.Plus, List(otherName: NameExpression, ConstantExpression(IntegerValue(1))))
          if assign.location == otherName => otherName
        case _ => return None
      }
      case _ => return None
    }

    if (incremented.name != name) {
      return None
    }

    // The variable must be in the range {start..end}
    Some((name, t, start, end))
  }

  override def visit(sections: OMPSections): Unit = {
    throw Failure("omp sections block is only allowed in omp parallel block");
  }

  override def visit(section: OMPSection): Unit = {
    throw Failure("omp section block is only allowed in omp sections block");
  }

  override def visit(par: OMPForSimd): Unit = {
    throw Failure("omp for simd cannot be at the top level");
  }

  override def visit(fr: OMPFor): Unit = {
    throw Failure("omp for cannot be at the top level");
  }

  def tryParallel(loop: LoopStatement): Option[(Seq[DeclarationStatement], ASTNode, Contract)] = {
    getParDeclFromLoop(loop) match {
      case None => None
      case Some((name, t, start, end)) =>
        val decl = create field_decl(name, t, create expression(StandardOperator.RangeSeq, start, end))

        if (FeatureScanner.isIterationContract(loop.getContract)) {
          Some((Seq(decl), rewrite(loop.getBody), loop.getContract))
        } else if (loop.getContract == null || loop.getContract.isEmpty) {
          val body = loop.getBody match {
            case block: BlockStatement if block.size == 1 => block.get(0)
            case other => other
          }
          body match {
            case loop: LoopStatement => tryParallel(loop) match {
              case Some((decls, body, contract)) =>
                Some((decl +: decls, body, contract))
              case None => None
            }
            case _ => None
          }
        } else {
          None
        }
    }
  }

  override def visit(loop: LoopStatement): Unit = {
    tryParallel(loop) match {
      case Some((decls, body, contract)) =>
        result = create region(null, create parallel_block("auto", contract, decls.toArray, body.asInstanceOf[BlockStatement]))
      case None =>
        super.visit(loop)
    }
  }

  private def getSimdLen(options: Seq[OMPOption]): Option[Int] = {
    options.foreach {
      case OMPSimdLen(i) => return Some(i)
      case _ =>
    }
    None
  }

  /* AST of parallel blocks than can be fused, or composed in parallel or sequentially. */
  sealed trait PPL {
    def hasStaticSchedule: Boolean

    def isNoWait: Boolean

    def isFor: Boolean

    def toOrdered: Seq[PPLBlock]

    def getDeps: Set[PPLDep]

    def toCOL(contract: Contract): ASTNode = {
      val blocks = toOrdered
      val labels = (blocks zip blocks.indices.map("omp_" + _.toString)).toMap
      val deps = getDeps

      create region(contract, blocks.map((block) => {
        block.toParBlock(
          labels(block),
          deps.filter(_.next == block).map(_.asCOLDep(labels)).toArray)
      }): _*)
    }
  }

  sealed abstract class PPLOp(p1: PPL, p2: PPL) extends PPL {
    override def hasStaticSchedule: Boolean = p1.hasStaticSchedule && p2.hasStaticSchedule

    override def isNoWait: Boolean = p1.isNoWait && p2.isNoWait

    override def isFor: Boolean = false

    override def toOrdered: Seq[PPLBlock] = p1.toOrdered ++ p2.toOrdered
  }

  /* Composition is translated to dependencies, either on a particular iteration or a whole block */
  sealed abstract class PPLDep(val first: PPLBlock, val next: PPLBlock) {
    def asCOLDep(labels: Map[PPLBlock, String]): ASTNode
  }

  case class PPLBlock(decls: Seq[DeclarationStatement],
                      body: BlockStatement,
                      contract: Contract,
                      loop: OMPLoop) extends PPL {
    override def hasStaticSchedule: Boolean = loop.options.exists {
      case OMPSchedule(OMPStatic) => true;
      case _ => false
    }

    override def isNoWait: Boolean = loop.options.exists {
      case OMPNoWait => true;
      case _ => false
    }

    /* I believe this is how it's meant in the paper, but I don't think it's the right abstraction. */
    override def isFor: Boolean = loop match {
      case OMPFor(_, _) => true
      case OMPParallelFor(_, _) => false
      case OMPForSimd(_, _) => false
    }

    override def toOrdered: Seq[PPLBlock] = Seq(this)

    def toParBlock(label: String, deps: Array[ASTNode]): ParallelBlock = {
      create parallel_block(label, contract, decls.toArray, body, deps)
    }

    override def getDeps: Set[PPLDep] = Set()
  }

  case class PPLSeq(p1: PPL, p2: PPL) extends PPLOp(p1, p2) {
    override def getDeps: Set[PPLDep] = {
      p1.getDeps ++ p2.getDeps ++
        (for (from <- p1.toOrdered; next <- p2.toOrdered) yield PPLBlockDep(from, next)).toSet
    }
  }

  case class PPLFuse(p1: PPL, p2: PPL) extends PPLOp(p1, p2) {
    override def getDeps: Set[PPLDep] = {
      p1.getDeps ++ p2.getDeps + PPLIterDep(p1.toOrdered.last, p2.toOrdered.head)
    }
  }

  case class PPLPar(p1: PPL, p2: PPL) extends PPLOp(p1, p2) {
    override def getDeps: Set[PPLDep] = p1.getDeps ++ p2.getDeps
  }

  case class PPLIterDep(override val first: PPLBlock, override val next: PPLBlock) extends PPLDep(first, next) {
    override def asCOLDep(labels: Map[PPLBlock, String]): ASTNode =
      create invokation(null, null, labels(first), create unresolved_name first.decls.head.name)
  }

  case class PPLBlockDep(override val first: PPLBlock, override val next: PPLBlock) extends PPLDep(first, next) {
    override def asCOLDep(labels: Map[PPLBlock, String]): ASTNode =
      create unresolved_name labels(first)
  }
}
