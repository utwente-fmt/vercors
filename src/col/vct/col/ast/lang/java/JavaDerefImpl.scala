package vct.col.ast.lang.java

import hre.util.FuncTools
import vct.col.ast._
import vct.col.print._
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.col.ast.ops.JavaDerefOps

trait JavaDerefImpl[G] extends JavaDerefOps[G] { this: JavaDeref[G] =>
  override lazy val t: Type[G] = ref.get match {
    case RefModelField(decl) => decl.t
    case ref: RefUnloadedJavaNamespace[G] => Types.notAValue(ref)
    case ref: RefJavaClass[G] => Types.notAValue(ref)
    case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
    case ref: RefModel[G] => Types.notAValue(ref)
    case RefVariable(v) => v.t
    case RefJavaField(decls, idx) => FuncTools.repeat[Type[G]](TArray(_), decls.decls(idx).moreDims, decls.t)
    case RefEnumConstant(enum, _) => TEnum(enum.get.ref)
    case BuiltinField(f) => f(obj).t
    case ref: RefEnum[G] => Types.notAValue(ref)
    case ref: RefProverType[G] => Types.notAValue(ref)
  }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(obj) <> "." <> field
}