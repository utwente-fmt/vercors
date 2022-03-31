package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaClass, JavaFields, JavaStatic, Type}
import vct.col.resolve.RefJavaField

trait JavaClassImpl[G] { this: JavaClass[G] =>
  override def supports: Seq[Type[G]] = ext +: imp

  def getField(name: String): Option[JavaFields[G]] = decls.collectFirst {
    case fields: JavaFields[G] if fields.decls.exists(_._1 == name) => fields
  }

  def getFieldRef(name: String): Option[RefJavaField[G]] = getField(name).map({
    case fields => RefJavaField(fields, fields.decls.indexWhere(_._1 == name))
  })

  def getInstanceField(name: String): Option[RefJavaField[G]] = getFieldRef(name).collect({
    case r @ RefJavaField(decls, idx) if !decls.isStatic => r
  })

  def getClassField(name: String): Option[RefJavaField[G]] = getFieldRef(name).collect({
    case r @ RefJavaField(decls, idx) if decls.isStatic => r
  })
}