package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.ast.{Class, Declaration, GlobalDeclaration, Procedure, TClass, Type}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object RemoveSelfLoops extends RewriterBuilder {
  override def key: String = "removeSelfLoops"

  override def desc: String = "Removing unnecessary self loop by removing Object class and corresponding procedure"
}


case class RemoveSelfLoops[Pre <: Generation]() extends Rewriter[Pre] {


  /**
   * Removes the extra Object class from all classes
   * @param decl
   */
  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: Procedure[Pre] => {
        p.returnType match {
          case c: TClass[Pre] => c.cls.decl.o.getPreferredNameOrElse().ucamel match {
            case "Object" =>
            case _ => rewriteDefault(p)
          }
          case _ => rewriteDefault(p)
        }
      }
      case c: Class[Pre] =>
        val preferredName = c.o.getPreferredNameOrElse().ucamel
        preferredName match {
          case "Object" =>
          case _ => globalDeclarations.succeed(c, dispatchGivenClass(c))
        }
      case _ => {
        super.rewriteDefault(decl)
      }
    }
  }

  /**
   * Recreates classes and removes the Object class as extension
   * @param c
   * @return
   */
  def dispatchGivenClass(c: Class[Pre]): GlobalDeclaration[Post] = {
    val newClass = c.rewrite(
      supports = createClassSupports(c)
    )
    newClass
  }

  /**
   * Filters the Object class from the supports list and returns the rest
   * @param c
   * @return
   */
  def createClassSupports(c: Class[Pre]): Seq[Type[Post]] =
    c.supports.filter {
      case TClass(Ref(cls), _) if cls.o.getPreferredNameOrElse().ucamel == "Object" => false
      case _ => true
    }.map(dispatch)
}