package vct.rewrite.runtime

import vct.col.ast.{Class, ClassDeclaration, CodeString, CodeStringClass, Declaration, InstanceField}
import vct.col.ast.RewriteHelpers.RewriteClass
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.rewrite.runtime.util.{CodeStringDefaults, FieldNumber}

object CreateFieldPermissions extends RewriterBuilder {
  override def key: String = "createArrayPermissions"

  override def desc: String = "Create permissions for items in arrays"
}


case class CreateFieldPermissions[Pre <: Generation]() extends Rewriter[Pre] {


  def createPermissionField(cls: Class[Pre], size: Int): ClassDeclaration[Post] = {
    val allNewHashMaps: Seq[String] = (1 to size).map(_ => CodeStringDefaults.newFieldConcurrentArray)
    new CodeStringClass[Post](CodeStringDefaults.newFieldPermissions(allNewHashMaps.mkString(", ")))(cls.o)
  }

  def dispatchClassDeclarations(cls: Class[Pre]): Seq[ClassDeclaration[Post]] = {
    classDeclarations.collect {
      val numberOfInstanceFields = cls.declarations.collect { case i: InstanceField[Pre] => i }.size
      cls.declarations.foreach(d => rewriteDefault(d))
      classDeclarations.declare(createPermissionField(cls, numberOfInstanceFields))
    }._1
  }

  def dispatchGivenClass(cls: Class[Pre]): Class[Post] = {
    val newClass = new RewriteClass[Pre, Post](cls).rewrite(
      declarations = dispatchClassDeclarations(cls)
    )
    newClass
  }


  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => globalDeclarations.succeed(cls, dispatchGivenClass(cls))
      case _ => super.rewriteDefault(decl)
    }
  }
}