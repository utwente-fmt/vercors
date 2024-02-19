package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

object GenerateJava extends RewriterBuilder {
  override def key: String = "generateJava"

  override def desc: String = "Create permissions for items in arrays"
}


case class GenerateJava[Pre <: Generation]() extends Rewriter[Pre] {


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    super.dispatch(program)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p:Procedure[Pre] => p.drop()
      case _ => super.dispatch(decl)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case pe@PreAssignExpression(_, p: ProcedureInvocation[Pre]) => pe.rewrite(value = procedureInvocationToNewObject(p))
      case pe@PostAssignExpression(_, p: ProcedureInvocation[Pre]) => pe.rewrite(value = procedureInvocationToNewObject(p))
      case _ => super.dispatch(e)
    }
  }

  override def dispatch(node: Statement[Pre]): Statement[Post] = {
    node match {
      case a@Assign(_, p: ProcedureInvocation[Pre]) => a.rewrite(value = procedureInvocationToNewObject(p))
      case _ => super.dispatch(node)
    }
  }

  private def procedureInvocationToNewObject(p: ProcedureInvocation[Pre]): NewObject[Post]= {
    val classDecl: Class[Pre] = p.ref.decl.returnType.asClass.get.cls.decl
    NewObject[Post](this.anySucc(classDecl))(classDecl.o)
  }

}