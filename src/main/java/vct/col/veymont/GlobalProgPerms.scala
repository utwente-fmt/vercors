package vct.col.veymont

import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr
import vct.col.ast.expr.{Dereference, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, ASTFlags, ASTSpecial, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}

import java.util
import scala.jdk.CollectionConverters.IterableHasAsScala

class GlobalProgPerms(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private var currentClassName : String = null
  private var currentMethodArgs : Array[DeclarationStatement] = null

  override def visit(c : ASTClass) : Unit = {
    currentClassName = c.name
    if(!Util.isChannelClass(c.name)) {
      val body = getFieldPerms(c.fields().asScala,true)
      val resource = create.predicate(Util.ownerShipPredicateName, body)
      resource.annotations().add(create.reserved_name(ASTReserved.Inline))
      if(c.name == Util.mainClassName)
        addIfNoMainMethod(c)
      c.add_dynamic(resource)
    }
    super.visit(c)
  }

  override def visit(m : Method): Unit = {
    currentMethodArgs = m.getArgs
    if(!Util.isChannelClass(currentClassName)) {
      var body = m.getBody
      val cb = new ContractBuilder()
      if (m.kind == Method.Kind.Constructor) {
        cb.requires(getMethodArgsPerms(m.getArgs))
        cb.ensures(create.invokation(null, null, Util.ownerShipPredicateName))
      } else if (m.kind == Method.Kind.Pure) {
        cb.requires(create.invokation(null, null, Util.ownerShipPredicateName))
        m.getArgs.filter(_.`type`.isInstanceOf[ClassType]).foreach(
          arg => cb.requires(create.invokation(create.argument_name(arg.name),null,Util.ownerShipPredicateName)))
      } else if (m.kind != Method.Kind.Predicate) {
        cb.requires(getMethodArgsPerms(m.getArgs))
        cb.context(create.invokation(null, null, Util.ownerShipPredicateName))
        body match {
          case block : BlockStatement => {
            val newBlock = new BlockStatement()
            for(s <- block.asScala) {
              s match {
                case i : IfStatement => if(currentClassName == Util.mainClassName) newBlock.add(create.special(ASTSpecial.Kind.Assert,getEquivCond(i.getGuard(0))))
                case _ => //nothing
              }
              newBlock.add(rewrite(s))
            }
            body = newBlock
          }
          case _ => //nothing
        }

      }
      if (m.getReturnType match {
        case p: PrimitiveType => p.sort != PrimitiveSort.Void && p.sort != PrimitiveSort.Resource && !StructureCheck.isAllowedPrimitive(p)
        case _ => true
      })
        cb.ensures(create.invokation(create.reserved_name(ASTReserved.Result), null, Util.ownerShipPredicateName))
      rewrite(m.getContract, cb)
      result = create.method_kind(m.kind, m.getReturnType, cb.getContract, m.name, m.getArgs, body)
    } else super.visit(m)
  }

  override def visit(l : LoopStatement) : Unit = {
    if(!Util.isChannelClass(currentClassName)) {
      val cb = new ContractBuilder()
      cb.appendInvariant(create.expression(StandardOperator.Star, create.expression(StandardOperator.Star,
        create.invokation(null, null, Util.ownerShipPredicateName),
        getMethodArgsPerms(currentMethodArgs)),
        getEquivCond(l.getEntryGuard)))
      rewrite(l.getContract, cb)
      result = create.while_loop(l.getEntryGuard, rewrite(l.getBody), cb.getContract)
    } else super.visit(l)
  }

  override def visit(p : ParallelRegion) : Unit = {
    val blockvars = p.blocks.map(b => getAssignmentVars(b.block))
    val (_, reads) = blockvars.unzip
    val counts = reads.flatten.map(_.toString).groupBy(identity).map { case (str, els) => (str, els.length) }
    val bl: List[(ParallelBlock, (Array[Dereference], Array[Dereference]))] = p.blocks.zip(blockvars)
    val contractblocks = bl.map { case (b: ParallelBlock, (writesb: Array[Dereference], readsb: Array[Dereference])) =>
      val cb = new ContractBuilder()
      (writesb.map(Util.getNameFromNode) ++ readsb.map(Util.getNameFromNode)).distinct.foreach(role =>
        cb.context(create.expression(StandardOperator.Perm,rewrite(role.get),
          create.expression(StandardOperator.Div,create.constant(1),create.constant(p.blocks.size))))
      )
      writesb.distinct.foreach(writevar => cb.context(create.expression(StandardOperator.Perm, rewrite(writevar),create.constant(1))))
      readsb.filter(!writesb.contains(_)).distinct.foreach(readvar => cb.context(create.expression(StandardOperator.Perm,rewrite(readvar),
        create.expression(StandardOperator.Div,create.constant(1),create.constant(if(counts(readvar.toString)==1) 2 else counts(readvar.toString))))))
      rewrite(b.contract,cb)
      cb.getContract
      create.parallel_block(b.label,cb.getContract,b.iters.toArray,rewrite(b.block))
    }
    result = create.region(rewrite(p.contract),contractblocks.toArray:_*)
  }

  private def getAssignmentVars(b : BlockStatement) : (Array[Dereference],Array[Dereference]) = {
    val (writes,reads) = b.getStatements.filter(_.isInstanceOf[AssignmentStatement]).map {
      case AssignmentStatement(location, expression) => location match {
        case dloc: Dereference => expression match {
          case dexp: Dereference => (dloc, Some(dexp))
          case o : OperatorExpression => getNodeFromOp(o).head match {
            case dexpvar : Dereference => (dloc,Some(dexpvar))
          }
          case _ => (dloc,None)
        }
      }
    }.unzip
    (writes,reads.filter(_.isDefined).map(_.get))
  }

  private def getNodeFromOp(n : ASTNode) : List[ASTNode] = n match {
    case op : OperatorExpression => op.args.flatMap(arg => getNodeFromOp(arg))
    case n : ASTNode => List(n)
  }

  private def getEquivCond(cond : ASTNode) : ASTNode = {
    val conds = ASTUtils.conjuncts(cond, StandardOperator.And).asScala.toArray
    if(conds.size > 1) {
      var exp: ASTNode = create.expression(StandardOperator.EQ, conds(0), conds(1))
      for (i <- 0 until conds.size; j <- 2 until conds.size; if i < j) {
        exp = create.expression(StandardOperator.And, exp, create.expression(StandardOperator.EQ, conds(i), conds(j)))
      }
      exp
    } else create.constant(true)
  }

  private def getMethodArgsPerms(args : Array[DeclarationStatement]) : ASTNode =
    getFieldPerms(args.filter(_.`type` match { //select heap locations
      case p: PrimitiveType => !StructureCheck.isAllowedPrimitive(p)
      case _ => true
    }), false)

  private def getFieldPerms(fields : Iterable[DeclarationStatement], isField : Boolean) : ASTNode = {
    val fieldPerms = {
      if (fields.isEmpty)
        List(create.constant(true))
      else fields.map(f => create.expression(StandardOperator.Perm, createFieldOrArgName(f.name, isField), create.fullPermission()))
    }
    val classFields = fields.filter(_.`type`.isInstanceOf[ClassType])
    val fieldClassPerms = classFields.map(f => classFieldPerm(createFieldOrArgName(f.name, isField)))
    val fieldPrimitiveTypes = fields.map(f => (f,Util.getArgPrimitiveSorts(f.getType)))
    val arrayPerms = fieldPrimitiveTypes.filter(_._2.contains(PrimitiveSort.Array)).map(t =>
      create.expression(StandardOperator.Star,
        create.expression(StandardOperator.NEQ,createFieldOrArgName(t._1.name,isField),create.reserved_name(ASTReserved.Null)),
        getforallArrayPerm(createFieldOrArgName(t._1.name,isField),None,t._2.count(p => p == PrimitiveSort.Array),t._2.last == PrimitiveSort.Cell)))
    (fieldPerms ++ fieldClassPerms ++ arrayPerms).reduce((p1, p2) => create.expression(StandardOperator.Star, p1, p2))
  }

  private def createFieldOrArgName(name : String, isField : Boolean) : NameExpression =
    if(isField) create.field_name(name) else create.argument_name(name)

  private def classFieldPerm(classFieldName : ASTNode) : ASTNode =
    create.invokation(classFieldName,null,Util.ownerShipPredicateName)

  private def getforallArrayPerm(arrayField : ASTNode, indexField : Option[ASTNode], depth : Int, isClassEl : Boolean): ASTNode = {
    if(depth == 0) {
      val arrayPerm = create.expression(StandardOperator.Perm,copy_rw.rewrite(arrayField),create.fullPermission())
      if(isClassEl) create.expression(StandardOperator.Star,arrayPerm,classFieldPerm(arrayField))
      else arrayPerm
    } else if (depth > 0) {
      val newIndex = "i" + depth.toString
      val guard = create.expression(StandardOperator.And,
        create.expression(StandardOperator.LTE,create.constant(0),create.local_name(newIndex)),
        create.expression(StandardOperator.LT,create.local_name(newIndex),
          create.dereference(arrayField,"length")))
      val newArrayField = create.expression(StandardOperator.Subscript,copy_rw.rewrite(arrayField),create.local_name(newIndex))
      create.starall(guard,
        getforallArrayPerm(newArrayField,Some(create.local_name(newIndex)),depth-1,isClassEl),
        create.field_decl(newIndex,create.primitive_type(PrimitiveSort.Integer)))
    } else throw Failure("VeyMont Fail: getForallArrayPerm called with negative depth")
  }

  private def addIfNoMainMethod(mainClass : ASTClass) : Unit = {
    mainClass.methods().asScala.find(_.name == Util.mainMethodName) match {
      case Some(_) => None
      case None => {
        val mainConstr = mainClass.methods().asScala.find(_.kind== Method.Kind.Constructor).get
        val mainPre = mainConstr.getContract.pre_condition
        val preContract = new ContractBuilder()
        preContract.requires(copy_rw.rewrite(mainPre))
        val mainArgs = copy_rw.rewrite(mainConstr.getArgs)
        val line1init = create.invokation(null,create.class_type(Util.mainClassName),
          Method.JavaConstructor,mainConstr.getArgs.map(a => create.argument_name(a.name)):_*)
        val localName = "seqProgram"
        val line1 = create.field_decl(localName,create.class_type(Util.mainClassName),line1init)
        val line2 = create.invokation(create.local_name(localName),null,Util.runMethodName)
        val mainBody = new BlockStatement()
        mainBody.add(line1)
        mainBody.add(line2)
        val mainMethod = create.method_kind(Method.Kind.Plain,create.primitive_type(PrimitiveSort.Void),preContract.getContract,Util.mainMethodName,mainArgs,mainBody)
        mainClass.add_dynamic(mainMethod)
      }
    }
  }

}
