package vct.col.util

import vct.col.ast.`type`.PrimitiveType
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, DeclarationStatement, Method, ProgramUnit, VariableDeclaration}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.RecursiveVisitor
import vct.col.util.SessionTerminationCheck.deadlockWarning
import vct.col.util.SessionUtil.{barrierClassName, channelClassName, mainClassName}

import scala.collection.JavaConversions._

object SessionTerminationCheck {
  private val deadlockWarning = " might not terminate, deadlock-freedom cannot be guaranteed! "
}

class SessionTerminationCheck(override val source : ProgramUnit) extends RecursiveVisitor(null, true) {

  private var encountered : Set[String] = Set()
  private var methodCalled = false
  private var currentClass : String = null
  private val methods =  getAllMethods(source)

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
      Warning("Session Warning: recursive call of method '%s'" + deadlockWarning, m.method, m.getOrigin)
    else methods.find(tup => tup._1.name == m.method) match {
      case Some(tup) => {
        val method = tup._1
        val mClass = tup._2
        if(mClass == mainClassName && currentClass != mainClassName) {
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
/*
  private def isForLoop(l : LoopStatement) : Boolean = {
    val guardOp = l.getEntryGuard.asInstanceOf[OperatorExpression]
    val initval = l.getInitBlock.asInstanceOf[BlockStatement].getStatement(0).asInstanceOf[VariableDeclaration].get().head.asInstanceOf[DeclarationStatement].init.get.asInstanceOf[ConstantExpression]

    val updateOp = l.getUpdateBlock.asInstanceOf[BlockStatement].getStatement(0)
    //TODO: get Value from initval
    if(isLengthExpr(guardOp.arg(1)))
      initval.value == 0 && Set(StandardOperator.LT).contains(guardOp.operator) && updateOp.operator == StandardOperator.PostIncr
    else initval.value < guardOp.arg(1).asInstanceOf[ConstantExpression].value && updateOp.operator == StandardOperator.PostIncr ||
      initval.value > guardOp.arg(1).asInstanceOf[ConstantExpression].value && updateOp.operator == StandardOperator.PostDecr
  }
*/
  private def isLengthExpr(e : ASTNode) : Boolean = e match {
    case d : Dereference => d.field == Dereference.ArrayLength
    case _ => false
  }

  //TODO: check declaration has int type
  private def isConstantInitDecl(i : ASTNode) : Boolean =
    i match {
      case ib : BlockStatement =>
        ib.size == 1 && (ib.getStatement(0) match {
          case init : VariableDeclaration =>
            init.basetype match {
              case p : PrimitiveType => p.isInteger && init.get().size == 1 && (init.get().head match {
                case d : DeclarationStatement => d.init match {
                  case Some(initval) => initval.isInstanceOf[ConstantExpression]
                  case None => false
                }
                case _ => false
              })
              case _ => false
            }
          case _ => false
        })
      case _ => false
    }

  private def isConstantOpGuard(g : ASTNode, itName : String) =
    g match {
      case op : OperatorExpression =>
        Set(StandardOperator.LT, StandardOperator.LTE,StandardOperator.GT, StandardOperator.GTE).contains(op.operator) &&
          op.argslength == 2 && isItName(op.arg(0), itName) && isConstantOrLength(op.arg(1))
      case _ => false
    }

  private def isConstantOrLength(e : ASTNode) : Boolean = e match {
    case c : ConstantExpression => true
    case d : Dereference => d.field == Dereference.ArrayLength
    case _ => false
  }

  private def isItName(e : ASTNode, itName : String) : Boolean = e match {
    case n : NameExpression => n.name == itName
    case _ => false
  }

  private def isConstantUpdate(u : ASTNode, itName : String) : Boolean = u match {
    case ub: BlockStatement =>
      ub.size == 1 && (ub.getStatement(0) match {
        case op : OperatorExpression => Set(StandardOperator.PostDecr, StandardOperator.PostIncr).contains(op.operator) && op.argslength == 1 && isItName(op.arg(0),itName)
        case a : AssignmentStatement => isItName(a.location,itName) && (a.expression match {
          case aop : OperatorExpression => Set(StandardOperator.Plus,StandardOperator.Minus).contains(aop.operator) &&
            aop.argslength == 2 && isItName(aop.arg(0), itName) && aop.arg(1).isInstanceOf[ConstantExpression]
        })
        case _ => false
      })
    case _ => false
  }


}
