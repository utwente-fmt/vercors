package vct.col.ast.`type`

import vct.col.ast.{TByReferenceClass, TByValueClass, TClass, TClassUnique, Type}
import vct.col.ast.ops.TClassUniqueOps
import vct.col.print._

trait TClassUniqueImpl[G] extends TClassUniqueOps[G] { this: TClassUnique[G] =>
  def cls = inner match {
    case TByReferenceClass(cls, _) => cls
    case TByValueClass(cls, _) => cls
  }

  def typeArgs: Seq[Type[G]] = inner match {
    case TByReferenceClass(_, tA) => tA
    case TByValueClass(_, tA) => tA
  }

  def innerT: TClass[G] = inner match {
    case t: TByReferenceClass[G] => t
    case t: TByValueClass[G] => t
  }

  override def layout(implicit ctx: Ctx): Doc = {
    uniqueMap.foldLeft[Doc](Text(ctx.name(cls))){case (i, (fref, unique)) =>
      Text("unique_class_field<") <> ctx.name(fref.decl) <> "," <> unique.toString <> ">" <+> i}
  }
}
