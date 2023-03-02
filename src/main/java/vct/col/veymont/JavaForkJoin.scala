package vct.col.veymont

import hre.ast.{MessageOrigin, Origin}
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType}
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.expr.{MethodInvokation, NameExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, ASTFlags, ASTSpecial, DeclarationStatement, Method, NameSpace, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTFactory, AbstractRewriter, ContractBuilder}
import vct.col.veymont.Util.{getBlockOrThrow, getNameFromNode, getNamesFromExpression, javaRecursiveActionClass}

import java.io.PrintWriter
import scala.jdk.CollectionConverters._

class JavaForkJoin(override val source: ProgramUnit)  extends AbstractRewriter(source) {

  private var parNr = 0
  private var classFields : Iterable[DeclarationStatement] = null

  override def visit(c : ASTClass) : Unit = {
    if(Util.isThreadClassName(c.getName)) {
      classFields = c.fields().asScala
      val thread = create.ast_class(c.name, c.kind,c.parameters,javaRecursiveActionClass +: c.super_classes,c.implemented_classes)
      for(item <- c.asScala) {
        thread.add(rewrite(item))
      }
      classFields = null
      result = thread
    } else if(c.getName == Util.localMainClassName) {
      val arrayArgs = Array(create.field_decl("args",create.primitive_type(PrimitiveSort.Array,create.primitive_type(PrimitiveSort.String))))
      val body = c.methods().asScala.find(_.kind == Method.Kind.Constructor) match {
        case Some(method: Method) => if(method.getArity == 0) {
          val bl = new BlockStatement()
          bl.add(create.new_object(create.class_type(Util.localMainClassName)))
          bl
        } else null
        case None => null
      }
      val mainargsmethod = create.method_decl(create.primitive_type(PrimitiveSort.Void),Array(create.class_type("InterruptedException")),
        new ContractBuilder().getContract,"main",arrayArgs, body)
      mainargsmethod.setFlag(ASTFlags.PUBLIC,true)
      c.add_static(mainargsmethod)
      result = c
    }
    else super.visit(c)
  }

  override def visit(m : Method) : Unit = {
    if(m.name == Util.runMethodName) {
      m.attach(create.reserved_name(ASTReserved.Protected))
      result = create.method_kind(m.kind, m.getReturnType, m.getContract, Util.javaRunMethodName, m.getArgs, rewrite(m.getBody))
    } else if(m.kind == Method.Kind.Predicate) {
      //remove all resources
    } else {
      if (m.name == Util.cloneMethod) {
        m.attach(create.reserved_name(ASTReserved.Protected))
      }
      super.visit(m)
    }
  }

  override def visit(b : BlockStatement) : Unit = {
    if(classFields != null && b.getLength > 0) {
      val stats = b.getStatements.map{
        case pr : ParallelRegion => pr.blocks.map(getForkFromParallelBlock)
        case n => List(rewrite(n))
      }.reduce((a,b) => a ++ b)
      result = create.block(stats:_*)
    } else super.visit(b)
  }

  def getForkFromParallelBlock(pb : ParallelBlock): MethodInvokation = {
    val parClassName = Util.parClassName + parNr
    parNr = parNr+1
    val parClass = create.ast_class(parClassName,ASTClass.ClassKind.Plain,Array.empty,Array(javaRecursiveActionClass),Array.empty)
    val void = create.primitive_type(PrimitiveSort.Void)
    val parBlockVars = classFields.filter(f => getParBlockVars(pb.block).exists(_.name == f.name))//.map(rewrite(_))
    val parClassConstrArgs = parBlockVars.map(v => create.field_decl(v.name + "Arg",v.`type`))
    val assignArgs = create.block(parBlockVars.map(arg => create.assignment(create.field_name(arg.name),create.argument_name(arg.name + "Arg"))).toArray:_*)
    val constr = create.method_kind(Method.Kind.Constructor,void,ContractBuilder.emptyContract(),parClassName,parClassConstrArgs.toArray,assignArgs)
    val compMethod = create.method_kind(Method.Kind.Plain,void,pb.contract,Util.javaRunMethodName,Array.empty,copy_rw.rewrite(pb.block))
    compMethod.attach(create.reserved_name(ASTReserved.Protected))
    parBlockVars.foreach(f => parClass.add_dynamic(copy_rw.rewrite(f)))
    parClass.add_dynamic(constr)
    parClass.add_dynamic(compMethod)
    target().add(parClass)
    val parClassCall = create.new_object(create.class_type(parClassName),parBlockVars.map(arg => create.field_name(arg.name)).toArray:_*)
    create.invokation(parClassCall,null,Util.javaThreadInvoke)
  }

  private def getParBlockVars(node : ASTNode) : List[NameExpression] = {
    node match {
      case b : BlockStatement => b.getStatements.map(getParBlockVars).reduce((a,b) => a ++ b)
      case a : AssignmentStatement => {
        val locRole = getNameFromNode(a.location).get
        val expName = getNamesFromExpression(a.expression)
        if(expName.isEmpty || expName.head.name == locRole.name)
          List(locRole)
        else List(locRole,expName.head)
      }
      case p : ParallelRegion => throw Failure("VeyMont Fail: Cannot handle nested par blocks in Java output generation! %s",p.getOrigin)
      case i : IfStatement => getParBlockVars(i.getStatement(0)) ++ (if(i.getCount > 1) getParBlockVars(i.getStatement(1)) else List.empty)
      case l : LoopStatement => getParBlockVars(l.getBody)
      case m : MethodInvokation => getNamesFromExpression(m).toList
      case _ : ASTSpecial => List.empty
      case _ => throw Failure("VeyMont Fail: Unexpected ASTNode in ParBlock: %s",node)
    }
  }
}
