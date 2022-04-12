package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaClass, PinnedDecl, JavaFields, JavaMethod, Type}
import vct.col.resolve.{RefJavaField, RefJavaMethod}

trait JavaClassImpl[G] { this: JavaClass[G] =>
  override def supports: Seq[Type[G]] = ext +: imp

  def getField(name: String): Option[JavaFields[G]] = decls.collectFirst {
    case fields: JavaFields[G] if fields.decls.exists(_.name == name) => fields
  }

  def getFieldRef(name: String): Option[RefJavaField[G]] = getField(name).map({
    case fields => RefJavaField(fields, fields.decls.indexWhere(_.name == name))
  })

  def getInstanceField(name: String): Option[RefJavaField[G]] = getFieldRef(name).collect({
    case r @ RefJavaField(decls, idx) if !decls.isStatic => r
  })

  def getClassField(name: String): Option[RefJavaField[G]] = getFieldRef(name).collect({
    case r @ RefJavaField(decls, idx) if decls.isStatic => r
  })

  def getMethods(name: String): Seq[RefJavaMethod[G]] = decls.collect {
    case m: JavaMethod[G] if m.name == name => RefJavaMethod(m)
  }
}