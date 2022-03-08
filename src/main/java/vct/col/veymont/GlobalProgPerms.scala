package vct.col.veymont

import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.StandardOperator
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.{ASTClass, ASTFlags, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}

import scala.jdk.CollectionConverters.IterableHasAsScala

class GlobalProgPerms(override val source: ProgramUnit) extends AbstractRewriter(null, true) {


  override def visit(c : ASTClass) : Unit = {
    if(!Util.isChannelClass(c.name)) {
      val fieldPerms = {
        if (c.fields().asScala.isEmpty)
          List(create.constant(true))
        else c.fields().asScala.map(f => create.expression(StandardOperator.Perm, create.field_name(f.name), create.fullPermission()))
      }
      val classFields = c.fields().asScala.filter(_.`type`.isInstanceOf[ClassType])
      val fieldClassPerms = classFields.map(f => classFieldPerm(create.field_name(f.name)))
      val fieldPrimitiveTypes = c.fields().asScala.map(f => (f,Util.getArgPrimitiveSorts(f.getType)))
      val arrayPerms = fieldPrimitiveTypes.filter(_._2.contains(PrimitiveSort.Array)).map(t =>
        create.expression(StandardOperator.Star,
          create.expression(StandardOperator.NEQ,create.field_name(t._1.name),create.reserved_name(ASTReserved.Null)),
          getforallArrayPerm(create.field_name(t._1.name),None,t._2.count(p => p == PrimitiveSort.Array),t._2.last == PrimitiveSort.Cell)))
      val body = (fieldPerms ++ fieldClassPerms ++ arrayPerms).reduce((p1, p2) => create.expression(StandardOperator.Star, p1, p2))
      val resource = create.predicate(Util.ownerShipPredicateName, body)
      resource.setFlag(ASTFlags.INLINE, true)
      if(c.name == Util.mainClassName)
        addIfNoMainMethod(c)
      c.add_dynamic(resource)
    }
    super.visit(c)
  }

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
