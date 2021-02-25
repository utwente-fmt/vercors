package vct.col.util

import vct.col.ast.expr.MethodInvokation
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, Method, ProgramUnit}
import vct.col.ast.util.RecursiveVisitor
import vct.col.util.SessionUtil.{barrierClassName, channelClassName, mainClassName}

import scala.collection.JavaConversions._

class SessionTerminationCheck(override val source : ProgramUnit) extends RecursiveVisitor(null, true) {

  private var encountered : Set[String] = Set()
  private var methodCalled = false
  private var currentClass : String = null
  private val methods =  getAllMethods(source)
    //SessionStructureCheck.getRoleOrHelperClasses(source) ++ SessionStructureCheck.getMainClass(source).methods().filter(_.kind == Method.Kind.Pure)

  private def getAllMethods(source : ProgramUnit) : Iterable[(Method,String)] =
    source.get().filter(_.isInstanceOf[ASTClass]).map(_.asInstanceOf[ASTClass])
      .filter(c => c.name != barrierClassName && c.name != channelClassName).flatMap(c => c.methods().map((_,c.name)))



  def checkTermination(): Unit = {
    methods.foreach(m => {
      encountered = Set()
      m._1.accept(this)
    })
  }


  override def visit(m : Method) : Unit = {
    m.getParent match {
      case c: ASTClass =>
        if (c.name != barrierClassName && c.name != channelClassName && (methodCalled || m.kind != Method.Kind.Pure)) {
          //only check called pure method  or non-pure method
          //don't check methods from Barrier or Channel class, so only methods from Main, roles, ot other classes
          if (methodCalled)
            methodCalled = false
          else
            encountered = Set()
          if (!(c.name == mainClassName && m.kind != Method.Kind.Pure)) //recursion is allowed in non-pure main methods
            encountered += m.name
          currentClass = c.name
          if(m.getBody != null) //abstract methods terminate anyway
            m.getBody.accept(this) //check method calls in all (main, role, and other class) method bodies
        }
      case _ => Fail("Session Fail: method '%s' doesn't have ASTCLass parent", m.name)
    }
  }

  override def visit(b : BlockStatement) : Unit = {
    val tmp = encountered
    b.getStatements.foreach(s => {
      encountered = tmp
      s.accept(this)
    })
  }

  override def visit(m : MethodInvokation) : Unit = {
    if(encountered.contains(m.method))
      Fail("Session Fail: recursion encountered for method '%s' at %s", m.method, m.getOrigin)
    else methods.find(tup => tup._1.name == m.method) match {
      case Some(tup) => {
        val method = tup._1
        val mClass = tup._2
        if(mClass == mainClassName && currentClass != mainClassName && method.kind != Method.Kind.Pure) {
          Fail("Session Fail: Cannot call Main method '%s' from role or other class! %s", m.method,m.getOrigin)
        } else if (mClass != mainClassName || method.kind == Method.Kind.Pure) {
          methodCalled = true
          encountered += method.name
          visit(method)
        } else {
          //stop checking; recursion is allowed in non-pure main methods
        }
      }
      case None => {
        if(!(m.method == Method.JavaConstructor && currentClass == mainClassName)) //Main constructor is fine, so skip
          Fail("Session Fail: Method invocation %s not allowed! %s", m.method, m.getOrigin)
      }
    }
  }

  override def visit(l : LoopStatement) : Unit =
    if(currentClass != mainClassName)
      Fail ("Session Fail: loop not allowed in method of non-Main class! " + l.getOrigin)

  override def visit(s : ASTSpecial) : Unit = {
    s.kind match {
      case ASTSpecial.Kind.Goto => Fail("Session Fail: Goto is not allowed! " + s.getOrigin)
      case ASTSpecial.Kind.Wait => Fail("Session Fail: Wait is not allowed! " + s.getOrigin)
      case ASTSpecial.Kind.Fork => Fail("Session Fail: Fork is not allowed! " + s.getOrigin)
      case ASTSpecial.Kind.Join => Fail("Session Fail: Join is not allowed! " + s.getOrigin)
      case ASTSpecial.Kind.Lock => Fail("Session Fail: Lock is not allowed! " + s.getOrigin)
      case ASTSpecial.Kind.Unlock => Fail("Session Fail: Unlock is not allowed! " + s.getOrigin)
      case ASTSpecial.Kind.Send => Fail("Session Fail: Send is not allowed! " + s.getOrigin)
      case ASTSpecial.Kind.Recv => Fail("Session Fail: Recv is not allowed! " + s.getOrigin)
      case _ => super.visit(s)
    }
  }

}
