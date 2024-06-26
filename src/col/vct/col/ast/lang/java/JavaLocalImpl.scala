package vct.col.ast.lang.java

import hre.util.FuncTools
import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.col.ast.ops.JavaLocalOps

trait JavaLocalImpl[G] extends JavaLocalOps[G] {
  this: JavaLocal[G] =>
  override lazy val t: Type[G] =
    ref.get match {
      case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
      case ref: RefEnum[G] => Types.notAValue(ref)
      case RefEnumConstant(decl, _) => TEnum(decl.get.ref)
      case RefVariable(decl) => decl.t
      case ref: RefUnloadedJavaNamespace[G] => Types.notAValue(ref)
      case ref: RefJavaClass[G] => Types.notAValue(ref)
      case RefJavaField(decls, idx) =>
        FuncTools.repeat[Type[G]](TArray(_), decls.decls(idx).moreDims, decls.t)
      case RefJavaLocalDeclaration(decls, idx) =>
        FuncTools.repeat[Type[G]](TArray(_), decls.decls(idx).moreDims, decls.t)
      case RefJavaParam(decl) => decl.t
      case RefJavaBipGuard(_) => TBool()
      case RefModelField(field) => field.t
      case ref: RefClass[G] => Types.notAValue(ref)
    }

  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
