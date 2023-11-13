package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.ast.{Class, Declaration, InstanceMethod, Program}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

object CreateForkPermissionTransfer extends RewriterBuilder {
  override def key: String = "createForkPermissionTransfrer"

  override def desc: String = "Detects fork methods and creates a permission transfer for the forked thread"
}


case class CreateForkPermissionTransfer[Pre <: Generation]() extends Rewriter[Pre] {

  val classes: ScopedStack[Class[Pre]] = ScopedStack()






  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }


  private def dispatchGivenClass(value: Class[Pre]): Class[Post] = {
    val newClass = new RewriteClass[Pre, Post](value).rewrite(
      declarations = classDeclarations.collect {
        value.declarations.foreach(d => dispatch(d))
      }._1
    )
    newClass
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => {
        classes.having(cls) {
          globalDeclarations.succeed(cls, dispatchGivenClass(cls))
        }
      }
      case im: InstanceMethod[Pre] => {
        im.o.getPreferredNameOrElse() match {
          case "run" => {

          }
          case _ => ()
        }
        super.dispatch(im)
      }
      case _ => super.dispatch(decl)
    }
  }
}