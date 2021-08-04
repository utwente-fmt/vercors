package vct.col.veymont

import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType}
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.{ASTClass, ASTFlags, Method, NameSpace, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}

import scala.jdk.CollectionConverters._

class JavaForkJoin(override val source: ProgramUnit)  extends AbstractRewriter(null, true) {

  override def visit(c : ASTClass) : Unit = {
    if(Util.isThreadClassName(c.getName)) {
      val recAct = ClassType(List("RecursiveAction"),List.empty)
      val thread = create.ast_class(c.name, c.kind,c.parameters,recAct +: c.super_classes,c.implemented_classes)
      for(item <- c.asScala) {
        thread.add(rewrite(item))
      }
      result = thread
    } else if(c.getName == Util.localMainClassName) {
      //def main(args: Array[String]): Unit =  { MainFJ(0, 8, 4)
      val arrayArgs = Array(create.field_decl("args",create.primitive_type(PrimitiveSort.Array,create.primitive_type(PrimitiveSort.String))))
      val body = new BlockStatement
      val mainFJArgTypes = c.methods().asScala.find(_.name == Util.localMainMethodName).get.getArgs.map(_.`type`)
      if(mainFJArgTypes.forall {
        case p: PrimitiveType => p.sort == PrimitiveSort.Boolean || p.sort == PrimitiveSort.Integer || p.sort == PrimitiveSort.Double
        case _ => false
      }) {
        val initArgs = mainFJArgTypes.map(p => p.asInstanceOf[PrimitiveType].sort match {
          case PrimitiveSort.Boolean => create.constant(true)
          case PrimitiveSort.Integer => create.constant(0)
          case PrimitiveSort.Double => create.constant(0.0)
        })
        val initBody = create.invokation(null,null,Util.localMainMethodName,initArgs:_*)
        body.add(initBody)
      }
      val mainargsmethod = create.method_decl(create.primitive_type(PrimitiveSort.Void),new ContractBuilder().getContract,"main",arrayArgs, body)
      mainargsmethod.setFlag(ASTFlags.PUBLIC,true)
      c.add_static(mainargsmethod)
      result = c
    }
    else super.visit(c)
  }

  override def visit(m : Method) : Unit = {
    if(m.name == Util.runMethodName) {
      m.attach(create.reserved_name(ASTReserved.Protected))
      result = create.method_kind(m.kind,m.getReturnType,m.getContract,Util.javaForkRunMethodName,m.getArgs,m.getBody)
    } else super.visit(m)
  }
}
