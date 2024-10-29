package vct.col.ast.lang.java

import vct.col.ast.{JavaClass, JavaFields, JavaMethod, Type}
import vct.col.print._
import vct.col.resolve.ctx._
import vct.col.util.AstBuildHelpers.tt
import vct.col.ast.ops.JavaClassOps

trait JavaClassImpl[G] extends JavaClassOps[G] {
  this: JavaClass[G] =>
  override def supports: Seq[Type[G]] = ext +: imp

  def getField(name: String): Option[JavaFields[G]] =
    decls.collectFirst {
      case fields: JavaFields[G] if fields.decls.exists(_.name == name) =>
        fields
    }

  def getFieldRef(name: String): Option[RefJavaField[G]] =
    getField(name).map({ case fields =>
      RefJavaField(fields, fields.decls.indexWhere(_.name == name))
    })

  def getInstanceField(name: String): Option[RefJavaField[G]] =
    getFieldRef(name).collect({
      case r @ RefJavaField(decls, idx) if !decls.isStatic => r
    })

  def getClassField(name: String): Option[RefJavaField[G]] =
    getFieldRef(name).collect({
      case r @ RefJavaField(decls, idx) if decls.isStatic => r
    })

  def getMethods(name: String): Seq[RefJavaMethod[G]] =
    decls.collect {
      case m: JavaMethod[G] if m.name == name => RefJavaMethod(m)
    }

  def layoutLockInvariant(implicit ctx: Ctx): Doc =
    Text("lock_invariant") <+> intrinsicLockInvariant <> ";"

  override def layout(implicit ctx: Ctx): Doc = {
    Doc.stack(Seq(
      if (intrinsicLockInvariant == tt[G])
        Empty
      else
        Doc.inlineSpec(Show.lazily(layoutLockInvariant(_))),
      Group(
        Doc.spread(modifiers :+ Text("class")) <+> name <>
          (if (typeParams.isEmpty)
             Empty
           else
             Text("<") <> Doc.args(typeParams) <> ">") <+> "extends" <+> ext <>
          (if (imp.isEmpty)
             Empty
           else
             Empty <+> "implements" <+> Doc.args(imp))
      ) <+> "{" <>> Doc.stack(decls) <+/> "}",
    ))
  }
}
