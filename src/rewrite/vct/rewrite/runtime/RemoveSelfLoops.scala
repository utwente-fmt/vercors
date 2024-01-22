package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.ast.{Class, Declaration, GlobalDeclaration, Procedure, TClass}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object RemoveSelfLoops extends RewriterBuilder {
  override def key: String = "removeSelfLoops"

  override def desc: String = "Removing unnecessary self loop by removing Object class and corresponding procedure"
}


case class RemoveSelfLoops[Pre <: Generation]() extends Rewriter[Pre] {


  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: Procedure[Pre] => {
        p.returnType match {
          case c: TClass[Pre] => c.cls.decl.o.getPreferredNameOrElse() match {
            case "Object" =>
            case _ => rewriteDefault(p)
          }
          case _ => rewriteDefault(p)
        }
      }
      case c: Class[Pre] =>
        val preferredName = c.o.getPreferredNameOrElse()
        preferredName match {
          case "Object" =>
          case _ => globalDeclarations.succeed(c, dispatchGivenClass(c))
        }
      case _ => {
        super.rewriteDefault(decl)
      }
    }
  }

  def dispatchGivenClass(c: Class[Pre]): GlobalDeclaration[Post] = {
    val newClass = new RewriteClass[Pre, Post](c).rewrite(
      supports = createClassSupports(c)
    )
    newClass
  }

  def createClassSupports(c: Class[Pre]): Seq[Ref[Post, Class[Post]]] = {
    c.supports.filter(ref => ref.decl.o.getPreferredNameOrElse() != "Object").map(
      element => {
        rewriter.porcelainRefSucc[Class[Post]](element).getOrElse(rewriter.succ[Class[Post]](element.decl))
      }
    )
  }
}