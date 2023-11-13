package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.ast.{Class, CodeStringClass, Declaration, InstanceField, InstanceMethod, Program, TArray}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.rewrite.runtime.util.{CodeStringDefaults, FieldNumber}
import vct.rewrite.runtime.util.CodeStringDefaults.newArrayPermission

object CreateArrayFieldPermission extends RewriterBuilder {
  override def key: String = "createArrayFieldPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class CreateArrayFieldPermission[Pre <: Generation]() extends Rewriter[Pre] {


  val classes: ScopedStack[Class[Pre]] = ScopedStack()


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }

  private def createPermissionField(instanceField: InstanceField[Pre]): Unit = {
    val cs = new CodeStringClass[Post](newArrayPermission(FieldNumber[Pre](classes.top).findNumber(instanceField)),instanceField.o.getPreferredNameOrElse())(instanceField.o)
    classDeclarations.declare(cs)
  }

  def dispatchGivenClass(cls: Class[Pre]): Class[Post] = {
    val newClass = new RewriteClass[Pre, Post](cls).rewrite(
      declarations = classDeclarations.collect {
        cls.declarations.foreach(d => dispatch(d))
      }._1
    )
    newClass
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {

    decl match {
      case instanceField: InstanceField[Pre] => {
        instanceField.t match {
          case array: TArray[Pre] => {
            createPermissionField(instanceField)
          }
          case _ =>
        }
        super.rewriteDefault(decl)
      }

      case cls: Class[Pre] => {
        classes.having(cls) {
          globalDeclarations.succeed(cls, dispatchGivenClass(cls))
        }
      }

      case _ => super.rewriteDefault(decl)
    }
  }
}