package vct.col.util

import vct.col.ast.expr.{Dereference, NameExpression, OperatorExpression}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.RecursiveVisitor
import vct.col.util.SessionStructureCheck.{getMainClass, getRoleObjectNames}
import vct.col.util.SessionUtil.getNamesFromExpression

import scala.collection.JavaConversions.iterableAsScalaIterable
/*
Check that expressions of assignments in the run method of the Main class don't mix names of different roles
 */
class SessionCommunicationCheck(override val source : ProgramUnit) extends RecursiveVisitor(null, true) {

  val roleNames = getRoleObjectNames(source)

  def checkMainMethodsAssignments(): Unit = {
    getMainClass(source).methods().filter(_.kind != Method.Kind.Constructor).foreach(_.accept(this))
  }

  override def visit(a : AssignmentStatement) = {
    val expNames = getNamesFromExpression(a.expression).map(_.name).toSet.filter(roleNames.contains(_))
    if(expNames.size > 1) {
      Fail("Session Fail: the assignment %s in a method of class 'Main' cannot have multiple roles in its expression.",a.toString)
    }
  }



}
