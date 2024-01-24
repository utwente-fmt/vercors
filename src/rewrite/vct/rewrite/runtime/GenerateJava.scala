package vct.rewrite.runtime

import vct.col.ast.{Class, ClassDeclaration, Declaration, JavaClass, Procedure, Program}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

object GenerateJava extends RewriterBuilder {
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class GenerateJava[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    super.dispatch(program)
  }

//  override def dispatch(decl: Declaration[Pre]): Unit = {
//    decl match {
//      case cls: Class[Pre] => {
//        val nd = dispatchClass(cls)
//        new JavaClass[Post](
//          cls.o.getPreferredNameOrElse(),
//          Seq.empty,
//          Seq.empty,
//          dispatch(cls.intrinsicLockInvariant),
//          null,
//          null,
//          nd)(null)(cls.o)
//      }
//      case p: Procedure[Pre] => dispatchProcedure(p)
//    }
//  }
//
//  def dispatchClass(cls: Class[Pre]): Seq[ClassDeclaration[Rewritten[Pre]]] = {
//    classDeclarations.collect {
//      cls.declarations.foreach(d => dispatch(d))
//    }._1
//  }
//
//  def dispatchProcedure(p: Procedure[Pre]): Unit = {
//
//  }
}