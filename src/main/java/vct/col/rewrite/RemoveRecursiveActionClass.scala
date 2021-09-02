package vct.col.rewrite

import vct.col.ast.expr.MethodInvokation
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, Method, ProgramUnit}
import vct.col.ast.util.AbstractRewriter
import vct.col.veymont.Util.{javaForkMethodName, javaJoinMethodName, javaRunMethodName}

import scala.collection
import scala.jdk.CollectionConverters.IterableHasAsScala

class RemoveRecursiveActionClass(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  override def visit(c : ASTClass) : Unit = {
    if(c.super_classes.exists(_.getName == "RecursiveAction")) {
      val newSignals =
        if (c.super_classes == null) return null
        else c.super_classes.filter(_.getName != "RecusiveAction")
      val astClass = create.ast_class(c.name,c.kind,rewrite(c.parameters),rewrite(c.implemented_classes),rewrite(newSignals))
      for (item <- c.asScala) {
        astClass.add(rewrite(item))
      }
    }
  }

  override def visit(m : MethodInvokation) : Unit = {
    if(m.method == javaForkMethodName) {
      result = create.special(ASTSpecial.Kind.Fork,m.`object`)
    } else if(m.method == javaJoinMethodName) {
      result = create.special(ASTSpecial.Kind.Join,m.`object`)
    } else super.visit(m)
  }

  override def visit(m : Method) : Unit = {
    if(m.name == javaRunMethodName) { //todo
      result = create.method_kind(m.kind, rewrite(m.getReturnType), rewrite(m.signals), rewrite(m.getContract()), "run", rewrite(m.getArgs), m.usesVarArgs, rewrite(m.getBody))
    } else super.visit(m)
  }
}
