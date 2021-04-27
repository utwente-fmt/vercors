package vct.col.rewrite

import hre.ast.MessageOrigin
import vct.col.ast.`type`.{ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr.{NameExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.util.SessionUtil.{barrierFieldName, getBarrierClass, getRoleName, isThreadClassName, mainClassName, mainMethodName, unArgName}

import scala.collection.convert.ImplicitConversions.`iterable AsScalaIterable`

class SessionStartThreadsClass(override val source: ProgramUnit)  extends AbstractRewriter(null, true) {


  def addStartThreadClass : ProgramUnit = {
    val threads = source.filter {
      case c: ASTClass => isThreadClassName(c.name)
      case _ => false
    }
    target.add(getStartThreadClass(threads.map(_.asInstanceOf[ASTClass]).toSet))
    rewriteAll()
  }

  private def getStartThreadClass(threads : Set[ASTClass]) = {
    val mainClass = create.new_class(mainClassName,null,null)
    val chansVars = threads.flatMap(getConstrChanArgs).map(getChanVar).toArray
    val barrierVar = getBarrierVar(threads.size)
    val threadVars = threads.map(t => getThreadVar(t,getConstrChanArgs(t).map(a => unArgName(a.name)))).toArray
    val threadForks = threads.map(t => getThreadRunning(t.name, true)).toArray
    val threadJoins = threads.map(t => getThreadRunning(t.name, false)).toArray
    val body = create.block(new MessageOrigin("Generated block of run method in Main class"),
      (barrierVar +: (chansVars ++ threadVars ++ threadForks ++ threadJoins)):_*)
    val void = create.primitive_type(PrimitiveSort.Void)
    val noArgs = Array() : Array[DeclarationStatement]
    val mainMethod = create.method_decl(void,new ContractBuilder().getContract,mainMethodName,noArgs,body)
    mainClass.add_static(mainMethod)
    mainClass
  }

  private def getBarrierVar(nrThreads : Int) : DeclarationStatement =
    create.field_decl(new MessageOrigin("Generated Barrier variable"),barrierFieldName,getBarrierClass(),
      create.invokation(null,getBarrierClass(),Method.JavaConstructor,create.constant(nrThreads)))

  private def getConstrChanArgs(thread : ASTClass) : Array[DeclarationStatement] =
    thread.methods().find(_.kind== Kind.Constructor).get.getArgs.tail

  private def getChanVar(chanArg : DeclarationStatement) : DeclarationStatement = {
    val chanType = create.class_type(chanArg.`type`.toString)
    create.field_decl(new MessageOrigin("Generated Channel variable"),unArgName(chanArg.name),chanType,
      create.invokation(null,chanType,Method.JavaConstructor))
  }

  private def getThreadVar(thread : ASTClass, chans : Iterable[String]) : DeclarationStatement = {
    val barArg = create.local_name(new MessageOrigin("Generated argument for calling constructor " + thread.name), barrierFieldName)
    val chanArgs = chans.map(chan => create.local_name(new MessageOrigin("Generated argument for calling constructor " + thread.name),chan)).toArray
    val args : Array[NameExpression] = barArg +: chanArgs
    create.field_decl(new MessageOrigin("Generated Thread variable"),getRoleName(thread.name),new ClassType(thread.name),
      create.invokation(null,new ClassType(thread.name),Method.JavaConstructor,args:_*))
  }

  private def getThreadRunning(threadClassName : String, isFork : Boolean) : ASTSpecial = {
    create.special(if(isFork) ASTSpecial.Kind.Fork else ASTSpecial.Kind.Join,
      create.local_name(new MessageOrigin("Generated argument for forking or joining"),getRoleName(threadClassName)))
  }
}
