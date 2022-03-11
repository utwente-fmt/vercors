package vct.col.veymont

import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.{NameExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTClass, ASTFlags, ASTSpecial, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}

import scala.jdk.CollectionConverters.IterableHasAsScala

class GlobalProgPerms(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  private var currentClassName : String = null
  private var currentMethodArgs : Array[DeclarationStatement] = null

  override def visit(c : ASTClass) : Unit = {
    currentClassName = c.name
    if(!Util.isChannelClass(c.name)) {
      val body = getFieldPerms(c.fields().asScala,true)
      val resource = create.predicate(Util.ownerShipPredicateName, body)
      resource.setFlag(ASTFlags.INLINE, true)
      if(c.name == Util.mainClassName)
        addIfNoMainMethod(c)
      c.add_dynamic(resource)
    }
    super.visit(c)
  }

  override def visit(m : Method): Unit = {
    currentMethodArgs = m.getArgs
    if(Util.isChannelClass(currentClassName))
      super.visit(m)
    else {
      var body = m.getBody
      val cb = new ContractBuilder()
      cb.requires(getMethodArgsPerms(m.getArgs))
      if (m.kind == Method.Kind.Constructor) {
        cb.ensures(create.invokation(null, null, Util.ownerShipPredicateName))
      } else if (m.kind == Method.Kind.Pure) {
        cb.requires(create.invokation(null, null, Util.ownerShipPredicateName))
      } else if (m.kind != Method.Kind.Predicate) {
        cb.context(create.invokation(null, null, Util.ownerShipPredicateName))
        body match {
          case block : BlockStatement => {
            val newBlock = new BlockStatement()
            for(s <- block.asScala) {
              s match {
                case i : IfStatement => newBlock.add(create.special(ASTSpecial.Kind.Assert,getEquivCond(i.getGuard(0))))
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
    }
  }

  override def visit(l : LoopStatement) : Unit = {
    val cb = new ContractBuilder()
    cb.appendInvariant(create.expression(StandardOperator.Star,create.expression(StandardOperator.Star,
      create.invokation(null, null, Util.ownerShipPredicateName),
      getMethodArgsPerms(currentMethodArgs)),
      getEquivCond(l.getEntryGuard)))
  }

  private def getEquivCond(cond : ASTNode) : ASTNode = {
    var exp : ASTNode = create.constant(true)
    val conds = ASTUtils.conjuncts(cond, StandardOperator.And).asScala.toArray
    for(i <- 0 until conds.size; j <- 0 until conds.size; if i != j) {
      exp = create.expression(StandardOperator.And, exp, create.expression(StandardOperator.EQ,conds(i),conds(j)))
    }
    exp
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

//  override def visit(m : Method) : Unit = {
////    Warning("GlobalProgPerms not implemented!")
//
//    val c = m.getContract()
//    val cb = new ContractBuilder()
//
//
//
//    //Warning(m.getType.toString)
//
////    val inv = create.invokation();
//
////    Warning("\n" + m.getContract.pre_condition.toString)
////    m.getContract.pre_condition.
//
////    m.setContract(...);
//
//    super.visit(m)
//  }

}
