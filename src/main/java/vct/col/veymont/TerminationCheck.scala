package vct.col.veymont

import vct.col.ast.expr._
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl._
import vct.col.ast.util.RecursiveVisitor
import Util.{isNoBarrierOrChannelClass, mainClassName}
import vct.col.veymont.TerminationCheck.deadlockWarning
import scala.jdk.CollectionConverters._

object TerminationCheck {
  private val deadlockWarning = " might not terminate, deadlock-freedom cannot be guaranteed! "
}

class TerminationCheck(override val source : ProgramUnit) extends RecursiveVisitor(source) {

  private var encountered = Set.empty[String]
  private var methodCalled = false
  private var currentClass : String = null
  private val methods =  getAllMethods(source)
  methods.foreach(m => {
    encountered = Set.empty
    m._1.accept(this)
  })

  private def getAllMethods(source : ProgramUnit) : Iterable[(Method,String)] =
    source.get().asScala.collect {
      case c: ASTClass if isNoBarrierOrChannelClass(c.name) => c.methods().asScala.map((_,c.name))
    }.flatten


  override def visit(m : Method) : Unit = {
    m.getParent match {
      case c: ASTClass =>
        if (isNoBarrierOrChannelClass(c.name) && (methodCalled || m.kind != Method.Kind.Pure) ) {
          //only check called pure method  or non-pure method
          //don't check methods from Barrier or Channel class, so only methods from Main, roles, ot other classes
          if (methodCalled)
            methodCalled = false
          else
            encountered = Set.empty
          encountered += m.name
          currentClass = c.name
          if(m.getBody != null) //abstract methods terminate anyway
            m.getBody.accept(this) //check method calls in all (main, role, and other class) method bodies
        }
      case _ => Fail("VeyMont Fail: method '%s' doesn't have a CLass parent", m.name)
    }
  }

  override def visit(b : BlockStatement) : Unit = {
    val tmp = encountered
    b.getStatements.foreach(s => {
      encountered = tmp
      s.accept(this)
    })
  }

  override def visit(op : OperatorExpression) : Unit = {
    val tmp = encountered
    op.args.foreach(s => {
      encountered = tmp
      s.accept(this)
    })
  }

  override def visit(m : MethodInvokation) : Unit = {
    if(encountered.contains(m.method)) {
      methods.find(tup => tup._1.name == m.method) match {
        case Some(tup) => {
          if(tup._2 == mainClassName)
            Fail("VeyMont Fail: recursive call %s in Main class not supported",m.method)
        }
        case None =>
      }
      Warning("Session Warning: recursive call of method '%s'" + deadlockWarning, m.method, m.getOrigin)
    } else methods.find(tup => tup._1.name == m.method) match {
      case Some(tup) => {
        val method = tup._1
        val mClass = tup._2
        if(mClass == mainClassName && currentClass != mainClassName) {
          Fail("VeyMont Fail: Cannot call Main method '%s' from role or other class! %s", m.method,m.getOrigin)
        } else {
          methodCalled = true
          encountered += m.method
          visit(method)
        }
      }
      case None => {
        if(!(m.method == Method.JavaConstructor && currentClass == mainClassName)) //Main constructor is fine, so skip
          Fail("VeyMont Fail: Method invocation %s not allowed! %s", m.method, m.getOrigin)
      }
    }
  }

  override def visit(l : LoopStatement) : Unit =
    if(currentClass != mainClassName)
      Warning("Session Warning: loop in method of non-Main class" + deadlockWarning)

  override def visit(s : ASTSpecial) : Unit = {
    s.kind match {
      case ASTSpecial.Kind.Goto => Warning("Session Warning: Goto" + deadlockWarning)
      case ASTSpecial.Kind.Wait => Warning("Session Warning: Wait" + deadlockWarning)
      case ASTSpecial.Kind.Fork => Warning("Session Warning: Fork" + deadlockWarning)
      case ASTSpecial.Kind.Join => Warning("Session Warning: Join" + deadlockWarning)
      case ASTSpecial.Kind.Lock => Warning("Session Warning: Lock" + deadlockWarning)
      case ASTSpecial.Kind.Unlock => Warning("Session Warning: Unlock" + deadlockWarning)
      case ASTSpecial.Kind.Send => Warning("Session Warning: Send" + deadlockWarning)
      case ASTSpecial.Kind.Recv => Warning("Session Warning: Recv" + deadlockWarning)
      case _ => super.visit(s)
    }
  }

}
