package vct.col.util

import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit}
import vct.col.ast.util.RecursiveVisitor
import vct.col.util.SessionUtil.mainClassName

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

class SessionCheck(override val source: ProgramUnit, val session: SessionRolesAndMain) extends RecursiveVisitor[AnyRef](null, true) {


  def check() : Unit = source.get().forEach(_.accept(this))

  //check that expressions do mix dereferences of different roles

  override def visit(c: ASTClass): Unit = {
    if(!c.getName.equals(mainClassName)) {
      methodsWriteToOwnFields(c)
    }
    //Fail("session fail!")
  }

  private def methodsUseMainTypeArgs(c : ASTClass) = {
    for(m <- c.methods()) {
      for(arg <- m.getArgs()) {
        if(arg.getType.toString.startsWith(mainClassName))
          Fail("Use of Main type not allowed")
      }
    }
  }

  private def methodsWriteToOwnFields(c : ASTClass)= {
    for(m <- c.methods()) {
      val wrongWrites = getWriteVars(m).filter(!isClassField(_,c))
      if(wrongWrites.nonEmpty)
        Fail("Role methods can only access own fields!" + wrongWrites.toString())
    }
  }

  private def isClassField(variable : ASTNode, c : ASTClass) = {
    variable match {
      case n : NameExpression => c.find_field(n.name) != null
      case _ => false
    }
  }

  private def getWriteVars(m : Method) : List[ASTNode] = {
    var writeVars: List[ASTNode] = List()
    m.getContract.pre_condition.annotations().forEach {
      case aop: OperatorExpression =>
        if (aop.operator == StandardOperator.Perm) {
          aop.second match {
            case frac: NameExpression =>
              if (frac.kind == NameExpressionKind.Reserved && frac.reserved == ASTReserved.FullPerm) {
                writeVars = aop.first :: writeVars
              }
          }
        }
    }
    writeVars
  }
}
